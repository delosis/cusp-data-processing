# Install Some packages - only needed if you don't have them! No need to rerun each time
list.of.packages <- c('httr', "data.table", "base64enc", "R.utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#' Get Questionnaire API authentication details
#' @return Named vector with authentication details
#' @keywords authentication
#' @export
get_questionnaire_auth <- function() {
  username <- Sys.getenv("QUESTIONNAIRE_API_USERNAME")
  password <- Sys.getenv("QUESTIONNAIRE_API_PASSWORD")
  server <- Sys.getenv("QUESTIONNAIRE_API_SERVER")
  
  if (username == "" || password == "" || server == "") {
    stop("Missing required environment variables for Questionnaire API authentication")
  }
  
  return(c(username = username, password = password, server = server))
}

#' Get Psytools API authentication details
#' @return Named vector with authentication details
#' @keywords authentication
#' @export
get_psytools_auth <- function() {
  username <- Sys.getenv("PSYTOOLS_API_USERNAME")
  password <- Sys.getenv("PSYTOOLS_API_PASSWORD")
  server <- Sys.getenv("PSYTOOLS_API_SERVER")
  
  if (username == "" || password == "" || server == "") {
    stop("Missing required environment variables for Psytools API authentication")
  }
  
  return(c(username = username, password = password, server = server))
}

#' Get session key for authentication
#' @return Character string containing session key
#' @export
get_session_key <- function() {
  auth <- get_questionnaire_auth()
  
  body.json = list(
    method = "get_session_key",
    id = " ",
    params = list(
      admin = auth["username"],
      password = auth["password"]
    )
  )
  
  r <- POST(paste0('https://', auth["server"], '/qs/admin/remotecontrol'), 
            content_type_json(),
            body = jsonlite::toJSON(body.json, auto_unbox = TRUE))
  
  session_key <- as.character(jsonlite::fromJSON(content(r, encoding="utf-8"))$result)
  if(session_key=='Invalid user name or password') {
    stop("Invalid user name or password")
  }
  return(session_key)
}

#' Convert base64 encoded data to data frame
#' @param x Base64 encoded string
#' @return Data frame
#' @export
base64_to_df <- function(x) {
  raw_csv <- rawToChar(base64enc::base64decode(x))
  return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = ","))
}

#' Download survey data from the server
#' @param surveyID ID of the survey to download
#' @param sDocumentType Type of document (default: "csv")
#' @param sLanguageCode Language code (default: "en")
#' @param sCompletionStatus Completion status (default: "all")
#' @param sHeadingType Heading type (default: "code")
#' @param sResponseType Response type (default: "short")
#' @return Data frame containing survey data
#' @export
downloadSurveyData <- function(surveyID, 
                              sDocumentType = "csv", 
                              sLanguageCode = 'en',
                              sCompletionStatus = "all", 
                              sHeadingType = "code",
                              sResponseType = "short") {
  
  params <- as.list(environment())
  params.full <- append(get_session_key(), params)
  body.json <- list(method = 'export_responses',
                    id = " ",
                    params = params.full)
  
  auth <- get_questionnaire_auth()
  r <- httr::POST(paste0('https://', auth["server"], '/qs/admin/remotecontrol'),
                  httr::content_type_json(),
                  body = jsonlite::toJSON(body.json, auto_unbox = TRUE))
  
  results <- jsonlite::fromJSON(httr::content(r, as='text', encoding="utf-8"))$result
  
  return(base64_to_df(unlist(results)))
}

#' Download Single Data File
#'
#' Download the specified task from the specified server
#'
#' @param taskDigestID taskID AND digest ID (eg TASK_ID-DIGEST_ID)
#' @param sampleID sampleID defaults to NULL
#' @return Data frame containing downloaded data
#' @keywords download dataset
#' @importFrom data.table fread
#' @import R.utils
#' @export
downloadSingleDataFile <- function(taskDigestID, sampleID=NULL) {
  auth <- get_psytools_auth()
  
  taskID <- taskDigestID
  if (!is.null(sampleID)) {
    taskID <- paste(taskID, sampleID, sep='-')
  }
  
  URL <- paste0('https://',URLencode(auth["username"], reserved=T), ':',URLencode(auth["password"], reserved=T), "@", auth["server"], 
                '/psytools-server/dataservice/dataset/',
                URLencode(paste0("CUSP-", taskID), reserved=TRUE), '.csv.gz')
  
  message("Attempting to download ", taskID)
  
  # Read directly from URL with authentication
  dt <- data.table::fread(URL,
                         stringsAsFactors=FALSE,
                         blank.lines.skip=TRUE,
                         na.strings="",
                         encoding="UTF-8",
                         colClasses = c(
                           "User code"="character",
                           "Iteration"="integer",
                           "Block"="character",
                           "Trial"="character",
                           "Response"="character",
                           "Response time [ms]"="numeric"))
  
  if (!is.null(dt)) {
    ## replace spaces and [] in column names to preserve compatibility with read.table
    names(dt) <- gsub(' ','.', names(dt))
    names(dt) <- gsub('\\[ms\\]', '.ms.', names(dt))
    
    if (nrow(dt)>0) {
      return(dt)
    } else {
      warning(paste(taskID, 'is empty - returning NULL'))
      return(NULL)
    }
  } else {
    stop(paste("Could not download dataset", taskID, "from server", auth["server"]))
  }
}

#' Escape special characters in strings
#' @param x String to escape
#' @return Escaped string
#' @export
escape <- function(x) {
  if (class(x) == "character") {
    # Escape double quotation marks by doubling them
    x <- gsub('"', '""', x)
    # Enclose in quotation marks strings with commas or quotation marks
    x <- gsub('^(.*[",].*$)', '"\\1"', x)
  }
  return (x)
}


#' Select Iteration from dataset
#'
#' Requires \code{User.code}, \code{Iteration}, \code{Completed}
#' Optionally \code{Valid}
#' columns in input data frame.
#'
#' By default returns the df trimmed to the first complete iteration for each user code
#'
#' @param df Data frame or table with simple questionnaire
#' @param iterationFunction function to apply to Iteration - default min
#' @param completed restrict to completed Iterations only - default TRUE
#' @param valid restrict to Iterations marked as valid IF the user.code has any valid attempts - default TRUE
#' @param allowIncomplete allow incomplete if a user.code has only incomplete data - default FALSE
#' @param allowInvalid allow invalid if a user.code has only invalid data - default TRUE
#' @param isQuestionnaire boolean indicating if this is questionnaire data - default TRUE
#' @return Data frame complying with input params
#' @export
selectIteration <- function(df,
                           iterationFunction = min,
                           completed = TRUE,
                           valid = FALSE,
                           allowIncomplete = FALSE,
                           allowInvalid = TRUE,
                           isQuestionnaire = TRUE) {
    
    if(is.null(df) || nrow(df) == 0) {
      return(df)
    }
    
    # remove the valid constraint if there is no Validity column in supplied DF
    if (valid & !("Valid" %in% colnames(df))) {
      warning("Validity selection requested but 'Valid' variable not supplied")
      valid <- FALSE
    }
    
    # Add an index to preserve order after the aggregation
    df$rowIndex <- seq_len(nrow(df))
    
    if (completed) {
      if (allowIncomplete) {
        # limit to Valid Iterations only if the User.code is ever valid
        df <- merge(df,
                   setNames(aggregate(Completed ~ User.code,
                                    max,
                                    data = df),
                           c("User.code", "everCompleted")),
                   by = c("User.code"),
                   sort = FALSE)
        df <- df[df$Completed == 't' | df$everCompleted == 'f',]
        df <- df[order(df$rowIndex),]
        df$everCompleted <- NULL
      } else {
        df <- df[df$Completed == 't',]
      }
    }
    
    if (valid) {
      if (allowInvalid) {
        # limit to Valid Iterations only if the User.code is ever valid
        df <- merge(df,
                   setNames(aggregate(Valid ~ User.code,
                                    max,
                                    data = df),
                           c("User.code", "everValid")),
                   by = c("User.code"),
                   sort = FALSE)
        df <- df[df$Valid == 't' | df$everValid == 'f',]
        df <- df[order(df$rowIndex),]
        df$everValid <- NULL
      } else {
        df <- df[df$Valid == 't',]
      }
    }
    
    if(nrow(df)==0){
      return(NULL)
    }
    
  

    # NB changed to using Completed.Timestamp instead of iteration to allow deduplicated user codes to 
    # have their multiple attempts correctly selected from ( iterations will be shared )
    if(isQuestionnaire){
      # remove skip_back flags
      df <- df[df$Trial.result !='skip_back',]
      df <- df[df$Trial.result != 'not_shown_back', ]
      # remove js flags
      df <- df[df$Block !='js',]
      # Select just the LAST response on each question 
      df <- df[!duplicated(subset(df, select=c(User.code, Iteration, Trial)), fromLast=T),]

    } else {
      df <- merge(df,
                  aggregate(Completed.Timestamp ~ User.code,
                          iterationFunction,
                          data = df),
                  by = c("User.code", "Completed.Timestamp"),
                  sort = FALSE)
      df <- df[order(df$rowIndex),]
      df$rowIndex <- NULL
    }
    return(df)
}

#' Rotate simple questionnaires from long to wide format.
#'
#' Requires \code{User.code}, \code{Iteration}, \code{Trial} and \code{Trial.result}
#' columns in input data frame.
#' Removes repeated occurrences of \code{Trial.result} caused by skipping back.
#'
#' @param df Data frame with simple questionnaire
#' @param BlockAsMeasureVar Default FALSE - Boolean indicating if Block should be used as measureVar
#' @param skippedValue Default NA - value for omitted questions
#' @param idVar list of variables to identify a row after rotation
#' @return Rotated data frame
#' @importFrom data.table dcast setDT setDF
#' @export
rotateQuestionnaire <- function(df,
                               BlockAsMeasureVar = FALSE,
                               skippedValue=NA,
                               idVar = c("User.code",
                                       "Iteration",
                                       "Language",
                                       "Completed",
                                       "Completed.Timestamp",
                                       "Processed.Timestamp")) {
    
    nonRequiredVars <- setdiff(c("User.code",
                                "Iteration", 
                                "Language",
                                "Completed",
                                "Completed.Timestamp",
                                "Processed.Timestamp",
                                "Trial",
                                "Trial.result"),
                              idVar)
    
    # replace skippedValue with session parameter if it exists
    if (exists('defaultUnadministeredValue')) {
      skippedValue <- defaultUnadministeredValue
    }
    
    measureVar = c("Trial")
    if (BlockAsMeasureVar) {
      measureVar = c("Block", measureVar)
    } else {
      nonRequiredVars <- c(nonRequiredVars, "Block")
    }
    
    if (sanityCheck(df, nonRequiredVars=nonRequiredVars) == FALSE) {
      warning("df does not meet requirements as passed")
      return(NULL)
    }
    
    #Keep Valid column if it exists
    if ("Valid" %in% colnames(df)) {
      idVar = c(idVar, "Valid")
    }
    
    df <- setDT(df)
    # prevent scientific notation as data.frames don't support them well
    options(scipen = 999)
    
    # Remove feedback results and skip back flags
    if ("Response" %in% names(df)) {
      df <- df[!grepl("FEEDBACK", df$Block, ignore.case = T) &
                 ( is.na(df$Response) | df$Response != 'skip_back') &
                 (is.na(df$Trial.result) | df$Trial.result != 'skip_back'), ]
    }
    
    # Select only last response for each question when skipping back
    df <- df[!duplicated(subset(df, select = c(head(idVar, 2), measureVar)), fromLast = T), ]
    
    if (sanityCheck(df, nonRequiredVars=nonRequiredVars) == FALSE) {
      stop("df does not meet requirements once filtered")
    }
    
    # Rotate and code skipped/unadministered variables with specified code
    df <- dcast(subset(df,
                      select = c(idVar, measureVar, "Trial.result")),
                as.formula(paste(paste(idVar, collapse = "+"),
                               "~" ,
                               paste(measureVar, collapse = "+"),
                               sep = " ")),
                fill = skippedValue,
                value.var = "Trial.result")
    
    return(setDF(fixNumericVariables(df)))
}

#' Check if a df meets minimum specs for processing
#' @param df Data frame to check
#' @param additionalVars extra columns that must be present
#' @param nonRequiredVars standard columns that don't need to be present
#' @return boolean indicating if df meets requirements
#' @export
sanityCheck <- function(df,
                       additionalVars = c(),
                       nonRequiredVars = c()) {
    reqVar = c("User.code",
               "Iteration",
               "Language",
               "Completed",
               "Completed.Timestamp",
               "Processed.Timestamp",
               "Block",
               "Trial",
               "Trial.result")
    reqVar <- setdiff(c(reqVar, additionalVars), nonRequiredVars)
    sane <- TRUE
    
    if (min(reqVar %in% colnames(df)) == 0) {
      sane <- FALSE
      warning("Columns do not meet requirements")
    }
    if (nrow(df) == 0) {
      sane <- FALSE
      warning("Data frame has no rows")
    }
    return(sane)
}

#' Coerce numeric vars encoded as character to numeric
#' @param df data frame to convert
#' @return data frame with numeric variables properly typed
#' @export
fixNumericVariables <- function(df) {
  df[] <- lapply(df, function(x) {
    if (suppressWarnings(!any(is.na(as.numeric(as.character(x))) - is.na((x))))) {
      as.numeric(as.character(x))
    } else {
      x
    }
  })
  return(df)
}
