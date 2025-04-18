# Load required libraries
library(stringdist)
library(data.table)

#' Find matching records based on criteria
#' @param .SD Data to match against
#' @param min_days Minimum days between records
#' @param max_days Maximum days between records
#' @param max_dist Maximum string distance for name matching
#' @param skipPotentialCheck Skip potential fake name check
#' @return Matching user code or NA
get_matching <- function(.SD, min_days, max_days, max_dist = 3, skipPotentialCheck = FALSE) {
  #remove fake names - leave potential ones for now
  if(!is.na(.SD$school) & !is.na(.SD$name) & 
     grepl('fake name', .SD$school, ignore.case=TRUE) & 
     !grepl('potential fake name', .SD$school, ignore.case=TRUE)) {
    .SD$name <- NA_character_
  }
    
  if(!skipPotentialCheck & !is.na(.SD$school) & !is.na(.SD$name) & 
     grepl('potential fake name', .SD$school, ignore.case=TRUE)) {
    # if this is a potential fake name try first allowing the fake names
    matchWithName <- get_matching(.SD, min_days, max_days, max_dist, TRUE)
    
    if(!is.na(matchWithName)) {
      return(matchWithName)
    } else {
      # remove the fake name 
      .SD$name <- NA_character_
    }
  }
  
  # Subset based on strict criteria (date of birth, gender) and time window
  # School is required if present in the source row
  subset_data <- matching[User.code != .SD$User.code &
                         (is.na(.SD$schoolID) | trimws(tolower(schoolID)) == trimws(tolower(.SD$schoolID))) &
                         RegSample == .SD$RegSample &
                         (B1=="REFUSE" | .SD$B1 =="REFUSE" | B1 == .SD$B1) &
                         (B5 %in% c("REFUSE", "4", "3") | .SD$B5 %in% c("REFUSE", "4", "3") | B5 == .SD$B5) &
                         as.numeric(date_of_testing - .SD$date_of_testing, units = "days") > min_days &
                         as.numeric(date_of_testing - .SD$date_of_testing, units = "days") < max_days]
  
  # remove fake names (but not potentials) from subset
  subset_data[grepl('fake name', school, ignore.case =TRUE) & 
              !grepl('potential fake name', school, ignore.case=TRUE),]$name <- NA_character_
  
  # If the subset has ANY rows and the source has a name then find the closest fuzzy match
  if (nrow(subset_data) > 0 & !is.na(.SD$name)) {
    flnames = strsplit(trimws(.SD$name), split= " ")
    max_dist <- nchar(flnames[[1]][1]) /4
    if(max_dist == 0) {max_dist <- 0.1}
    potential_match <- amatch(
      trimws(tolower(.SD$name)),
      trimws(tolower(subset_data$name)),
      maxDist = max_dist,
      method = "dl",
      weight = c(d = 0.5, i = 0.5, s = 1, t = 1),
    )
    if (!is.na(potential_match)) {
      if (trimws(tolower(.SD$name)) != trimws(tolower(subset_data[potential_match, name]))) {
        message(paste("Fuzzy Match Found:",.SD$name,"with", subset_data[potential_match, name]))
      }
      return(as.character(subset_data[potential_match, User.code]))
    } else {
      return(NA_character_)
    }
  } else {
    #if there is only one potential match (source name must be empty here - match MUST HAVE DOB)
    if(nrow(subset_data)==1 & subset_data[1, B1] != "REFUSE") {
      message(paste("Single nameless match:",.SD$User.code, "with", subset_data[1, User.code]))
      return(paste0('#NAMELESS#_',as.character(subset_data[1, User.code])))
    }
    if(nrow(subset_data)>1 & subset_data[1, B1] != "REFUSE") {
      message(paste("Multiple nameless match:",.SD$User.code, "with", subset_data[1, User.code]))
      return(paste0('#NAMELESSMULTIPLE#_',as.character(subset_data[1, User.code])))
    }
    return(NA_character_)
  }
}

#' Find matching records for NS samples
#' @param .SD Data to match against
#' @param min_days Minimum days between records
#' @param max_days Maximum days between records
#' @param max_dist Maximum string distance for name matching
#' @return Matching user code or NA
get_matching_ns <- function(.SD, min_days, max_days, max_dist = 0.20) {
  # Subset based on strict criteria (date of birth, gender) and time window
  subset_data <- matching[User.code != .SD$User.code &
                         RegSample == .SD$RegSample &
                         (B1=="REFUSE" | .SD$B1 =="REFUSE" | B1 == .SD$B1) &
                         (B5=="REFUSE" | .SD$B5 =="REFUSE" | B5 == .SD$B5) &
                         as.numeric(date_of_testing - .SD$date_of_testing, units = "days") > min_days &
                         as.numeric(date_of_testing - .SD$date_of_testing, units = "days") < max_days]
  
  if (nrow(subset_data) > 0) {
    print(paste(.SD$User.code, nrow(subset_data)))
    print(subset_data)
    return(as.character(subset_data[1, User.code]))
  } else {
    return(NA_character_)
  }
}

#' Extract nameless matches from matching data
#' @param matching Data frame containing matching data
#' @return Data frame with nameless matches
get_nameless_matches <- function(matching) {
  # Find rows where any match is nameless
  nameless <- matching[
    ((!is.na(AutoDuplicate) & grepl('#NAMELESS#', AutoDuplicate)) |
     (!is.na(Y2AutoMatch) & grepl('#NAMELESS#', Y2AutoMatch)) |
     (!is.na(Y3AutoMatch) & grepl('#NAMELESS#', Y3AutoMatch))),
    .(User.code, AutoDuplicate, Y2AutoMatch, Y3AutoMatch)
  ]
  return(nameless)
}

#' Find matches with date discrepancies
#' @param matching Data frame containing matching data
#' @param tolerance_days Number of days tolerance for year matching (default 30)
#' @return Data frame with date discrepancies
get_date_discrepancies <- function(matching, tolerance_days = 30) {
  tryCatch({
    # Function to check if dates are approximately N years apart
    check_year_diff <- function(date1, date2, years, tolerance) {
      # Handle NA values first
      na_mask <- is.na(date1) | is.na(date2)
      if (all(na_mask)) return(rep(FALSE, length(date1)))
      
      # Initialize result vector
      result <- rep(FALSE, length(date1))
      
      # Calculate differences only for non-NA pairs
      valid_pairs <- !na_mask
      if (any(valid_pairs)) {
        expected_days <- years * 365
        actual_days <- as.numeric(difftime(date2[valid_pairs], 
                                         date1[valid_pairs], 
                                         units = "days"))
        result[valid_pairs] <- abs(actual_days - expected_days) > tolerance
      }
      
      return(result)
    }
    
    # Create copy to avoid modifying original
    discrepancies <- copy(matching)
    
    # Add columns for matched dates
    discrepancies[, Y2_date := as.Date(matching[match(Y2AutoMatch, User.code), date_of_testing])]
    discrepancies[, Y3_date := as.Date(matching[match(Y3AutoMatch, User.code), date_of_testing])]
    
    # Find records with date discrepancies
    discrepancies <- discrepancies[
      ((!is.na(Y2AutoMatch) & check_year_diff(date_of_testing, Y2_date, 1, tolerance_days)) |
       (!is.na(Y3AutoMatch) & check_year_diff(date_of_testing, Y3_date, 2, tolerance_days))),
      .(User.code, date_of_testing, Y2AutoMatch, Y2_date, Y3AutoMatch, Y3_date)
    ]
    
    return(discrepancies)
  }, error = function(e) {
    message("Error in get_date_discrepancies: ", e$message)
    # Return empty data.table with correct structure
    return(data.table(
      User.code = character(),
      date_of_testing = as.Date(character()),
      Y2AutoMatch = character(),
      Y2_date = as.Date(character()),
      Y3AutoMatch = character(),
      Y3_date = as.Date(character())
    ))
  })
}

#' Resolve duplicates in matching data
#' @param matching Data frame containing matching data
#' @return List containing resolved data frame and duplicate mapping
#' @export
resolve_duplicates <- function(matching) {
  # Create copy to avoid modifying original
  resolved <- copy(matching)
  
  # Initialize duplicate mapping with columns for Y2/Y3 matches
  duplicate_map <- data.table(
    kept_id = character(),
    removed_id = character(),
    reason = character(),
    y2_match = character(),
    y3_match = character()
  )
  
  # Find all duplicate sets based on AutoDuplicate only
  # (Y2/Y3 matches are not duplicates, they're longitudinal data points)
  duplicates <- resolved[!is.na(AutoDuplicate), .(
    User.code,
    AutoDuplicate = gsub("^#NAMELESS(MULTIPLE)?#_", "", AutoDuplicate),
    date_of_testing
  )]
  
  # Process each set of duplicates
  for (i in 1:nrow(duplicates)) {
    dup_set <- duplicates[i]
    pair <- c(dup_set$User.code, dup_set$AutoDuplicate)
    
    # Get full records for the pair
    records <- resolved[User.code %in% pair]
    
    if (nrow(records) > 1) {
      # Keep the earliest record
      keep_idx <- which.min(records$date_of_testing)
      keep_record <- records[keep_idx]
      remove_records <- records[setdiff(1:nrow(records), keep_idx)]
      
      # For each removed record, check if it had Y2/Y3 matches
      for (i in 1:nrow(remove_records)) {
        removed <- remove_records[i]
        
        # If kept record has no Y2 match but removed record does, transfer it
        if (is.na(keep_record$Y2AutoMatch) && !is.na(removed$Y2AutoMatch)) {
          keep_record$Y2AutoMatch <- removed$Y2AutoMatch
        }
        
        # If kept record has no Y3 match but removed record does, transfer it
        if (is.na(keep_record$Y3AutoMatch) && !is.na(removed$Y3AutoMatch)) {
          keep_record$Y3AutoMatch <- removed$Y3AutoMatch
        }
        
        # Record mapping including Y2/Y3 matches
        duplicate_map <- rbind(duplicate_map, data.table(
          kept_id = keep_record$User.code,
          removed_id = removed$User.code,
          reason = "earliest",
          y2_match = ifelse(!is.na(removed$Y2AutoMatch), removed$Y2AutoMatch, NA_character_),
          y3_match = ifelse(!is.na(removed$Y3AutoMatch), removed$Y3AutoMatch, NA_character_)
        ))
      }
      
      # Update the kept record with any transferred Y2/Y3 matches
      resolved[User.code == keep_record$User.code, 
               `:=`(Y2AutoMatch = keep_record$Y2AutoMatch,
                    Y3AutoMatch = keep_record$Y3AutoMatch)]
      
      # Remove duplicates from resolved dataset
      resolved <- resolved[!(User.code %in% remove_records$User.code)]
    }
  }
  
  return(list(
    resolved = resolved,
    duplicate_map = duplicate_map
  ))
}

#' Save duplicate resolution mapping
#' @param duplicate_map Data frame containing duplicate mapping
#' @param date Current date for filename
#' @export
save_duplicate_map <- function(duplicate_map, date) {
  write.xlsx(
    duplicate_map,
    paste0("CUSP_DUPLICATE_MAP_", date, ".xlsx"),
    quote = FALSE,
    na = "",
    rowNames = FALSE
  )
}


#' Process and Anonymize School IDs
#' 
#' This function processes the input data frame by anonymizing school IDs, calculating age at testing,
#' and filtering relevant columns. It maintains a persistent mapping of school IDs to anonymized codes
#' across runs.
#' 
#' @param df A data.table containing the input data with columns including User.code, schoolID,
#'           date_of_testing, and B1 (birth date).
#' 
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Converts school IDs to lowercase
#'   \item Maintains a persistent mapping of school IDs to anonymized 3-digit codes
#'   \item Calculates age at testing using date_of_testing and birth date (B1)
#'   \item Filters and returns only relevant columns
#' }
#' 
#' @return A data.table with the following columns:
#' \itemize{
#'   \item User.code: Participant identifier
#'   \item schoolID: Anonymized 3-digit school code
#'   \item dataTag: Data tag identifier
#'   \item RegSample: Registration sample identifier
#'   \item District: District identifier
#'   \item ManualQC.SuggestedDuplicate: Suggested duplicate matches from manual QC
#'   \item AutoDuplicate: Automatically detected duplicate matches
#'   \item Y2AutoMatch: Year 2 automatic matches
#'   \item Y3AutoMatch: Year 3 automatic matches
#'   \item age_at_testing: Calculated age at time of testing (in years)
#' }
#' 
#' @examples
#' \dontrun{
#' # Process and anonymize matching data
#' matching_anon <- process_and_anonymize(matching)
#' }
#' 
#' @import data.table
#' @export
process_and_anonymize <- function(df) {
  # Define columns to keep
  keep_cols <- c("User.code", "schoolID", "dataTag", "RegSample", "District", "ManualQC.SuggestedDuplicate",
                "AutoDuplicate", "Y2AutoMatch", "Y3AutoMatch", "date_of_testing", "B1")
  
  # Initialize key file
  KEY_FILE <- "school_id_keys.env"
  
  # Convert school IDs to lowercase
  df$schoolID <- tolower(df$schoolID)
  
  # Load or create school key
  if(file.exists(KEY_FILE)) {
    school_key <- read.table(KEY_FILE, sep="=", 
                            col.names = c("schoolID", "anon_id"))
    school_key$schoolID <- tolower(school_key$schoolID)
  } else {
    school_key <- data.frame(schoolID=character(), anon_id=integer())
  }
  
  # Find new schools
  new_schools <- setdiff(unique(df$schoolID), school_key$schoolID)
  
  # Generate new codes if needed
  if(length(new_schools) > 0) {
    existing_codes <- school_key$anon_id
    available_codes <- setdiff(100:999, existing_codes)
    
    if(length(available_codes) < length(new_schools)) {
      stop("Insufficient available 3-digit codes for new schools")
    }
    
    set.seed(nchar(paste0(new_schools, collapse = "")))
    new_codes <- sample(available_codes, length(new_schools))
    
    # Update key
    school_key <- rbind(school_key,
                       data.frame(schoolID = new_schools,
                                 anon_id = new_codes))
    write.table(school_key, KEY_FILE, sep="=",
               col.names = FALSE, row.names = FALSE)
  }
  
  # Merge and anonymize
  df <- merge(df, school_key, by = "schoolID", all.x = TRUE)
  df$schoolID <- df$anon_id
  df$anon_id <- NULL
  
  # Calculate age at testing
  # Convert date_of_testing from DD/MM/YYYY to Date
  df[, date_of_testing := as.Date(date_of_testing, format = "%d/%m/%Y")]
  
  # Convert B1 (birth date) from DD-MM-YYYY to Date
  df[, birth_date := as.Date(B1, format = "%d-%m-%Y")]
  
  # Calculate age at testing in years
  df[, age_at_testing := as.numeric(difftime(date_of_testing, birth_date, units = "days")) / 365.25]
  
  # Round to 2 decimal places
  df[, age_at_testing := round(age_at_testing, 2)]
  
  # Remove temporary columns and date_of_testing
  df[, c("birth_date", "B1", "date_of_testing") := NULL]
  
  # Add age_at_testing to keep_cols
  keep_cols <- c(keep_cols[!keep_cols %in% c("date_of_testing", "B1")], "age_at_testing")
  
  # Filter columns
  df <- df[, ..keep_cols]
  
  return(df)
}


#' Process and combine questionnaires with error handling
#' @param duplicate_map Data frame containing duplicate mapping
#' @param Q1 Q1 data frame
#' @param Q2 Q2 data frame
#' @param current_date Current date for filename
#' @return List of processed questionnaires by site
#' @export
process_questionnaires <- function(duplicate_map, Q1, Q2, current_date) {
  tryCatch({
    message("Starting questionnaire processing...")
    
    # Initialize result list
    result <- list()
    
    message("Downloading Q3 data...")
    Q3 <- tryCatch({
      q3_base <- selectIteration(
        downloadSingleDataFile("CUSP_Q3-BASIC_DIGEST"),
        valid = FALSE
      )
      setDT(q3_base)
      
      q3_ubco <- selectIteration(
        downloadSingleDataFile("CUSP_UBCO_DELTA_Q3-BASIC_DIGEST"),
        valid = FALSE
      )
      

      
      rbindlist(list(q3_base, q3_ubco), fill = TRUE)
    }, error = function(e) {
      message("Error downloading Q3 data: ", e$message)
      return(data.table())
    })
    
    message("Downloading Q4 data...")
    Q4 <- tryCatch({
      q4_base <- selectIteration(
        downloadSingleDataFile("CUSP_Q4-BASIC_DIGEST"),
        valid = FALSE
      )
      setDT(q4_base)
      
      q4_ontario <- selectIteration(
        downloadSingleDataFile("CUSP_ONTARIO_Q4-BASIC_DIGEST"),
        valid = FALSE
      )
      
      rbindlist(list(q4_base, q4_ontario), fill = TRUE)
    }, error = function(e) {
      message("Error downloading Q4 data: ", e$message)
      return(data.table())
    })
    
    message("Combining questionnaires...")
    Qs <- rbindlist(list(Q1, Q2, Q3, Q4), fill = TRUE)
    
    message("Applying duplicate mapping...")
    for (i in 1:nrow(duplicate_map)) {
      Qs[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
    }
    
    message("Cleaning and deduplicating...")
    Qs <- Qs[User.code != 'DEV', ]
    # Sort by Completed.Timestamp to ensure we keep the most recent entries when they have repeated a question.
    setorder(Qs, User.code, Trial, Completed.Timestamp)
    Qs <- Qs[Qs$Trial.result != 'skip_back', ]
    Qs <- Qs[Qs$Block != 'js', ]
    Qs <- Qs[!duplicated(subset(Qs, select = c(User.code, Trial)), fromLast = T), ]
    
    
    message("Rotating to wide format...")
    
    # Extract date part from Completed.Timestamp
    Qs$Date.Completed <- as.Date(Qs$Completed.Timestamp)

    # Rotate questionnaire with specific idVar
    Qs <- rotateQuestionnaire(Qs, idVar = c("User.code", "Language", "Date.Completed"))
    setDT(Qs)

    setDT(registrationDF)
    #Merge in RegSample info
    Qs <- Qs[registrationDF[,c("User.code", "name", "RegSample", "District")], on = c(User.code = "User.code")]
    
    # Remove any no data cases and test registrations
    Qs <- Qs[!is.na(Date.Completed) &
           !grepl("TEST", name, ignore.case = T) , ]
    
    ## Add derived variables
    Qs<-deriveCUSPSURPS(Qs, TRUE, FALSE)


    message("Splitting by site and saving...")
    sites <- c("CUSP_NS", "CUSP_UBCO", "CUSP_ONTARIO")
    
    for (site in sites) {
      tryCatch({
        site_data <- Qs[RegSample == site, -c("RegSample", "name", "District")]
        write.xlsx(
          site_data,
          paste0("CUSP_QUESTIONNAIRES_", site, "_", current_date, ".xlsx"),
          quote = FALSE,
          na = "",
          rowNames = FALSE
        )
        result[[site]] <- site_data
      }, error = function(e) {
        message("Error processing site ", site, ": ", e$message)
      })
    }
    
    message("Generating summaries...")
    Qs$SchoolYear <- NA_character_
    Qs$SchoolYear[Qs$Processed.Timestamp > '2020-08-01'] <- "20-21"
    Qs$SchoolYear[Qs$Processed.Timestamp > '2021-08-01'] <- "21-22"
    Qs$SchoolYear[Qs$Processed.Timestamp > '2022-08-01'] <- "22-23"
    Qs$SchoolYear[Qs$Processed.Timestamp > '2023-08-01'] <- "23-24"
    
    # Generate site-level summary
    site_summary <- tryCatch({
      setDT(Qs)
SUMMARY <- Qs[, list(
  N = nrow(.SD),
  pctAlcohol12m_C1 = 100 * sum(C1A_A == "1", na.rm = T) / sum(!is.na(.SD$C1A_A)),
  pctCannabis12m_C1 = 100 * sum(C1A_B == "1", na.rm = T) / sum(!is.na(.SD$C1A_B)),
  pctAlcohol12m_C2 = 100 * sum(C2_A %in% c("2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_A)),
  pctCannabis12m_C2 = 100 * sum(C2_D %in% c("2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_D)),
  pctAlcoholEver_C2 = 100 * sum(C2_A %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_A)),
  pctTobaccoEver_C2 = 100 * sum(C2_B %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_B)),
  pctEcigEver_C2 = 100 * sum(C2_C %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_C)),
  pctCannabisEver_C2 = 100 * sum(C2_D %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_D)),
  pctSpiceEver_C2 = 100 * sum(C2_E %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_E)),
  pctCoughEver_C2 = 100 * sum(C2_F %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_F)),
  pctPainEver_C2 = 100 * sum(C2_G %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_G)),
  pctStimulantEver_C2 = 100 * sum(C2_H %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_H)),
  pctSedativeEver_C2 = 100 * sum(C2_I %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_I)),
  pctCocaineEver_C2 = 100 * sum(C2_K %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_K)),
  pctFoilEver_C2 = 100 * sum(C2_W %in% c("1", "2", "3", "4", "5", "6"), na.rm = T) / sum(!is.na(.SD$C2_W)),
  pctD2_01 = 100 * sum(D2_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_01)),
  pctD2_01_MonthlyPlus = 100 * sum(D2_01 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_01)),
  pctD2_02 = 100 * sum(D2_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_02)),
  pctD2_02_MonthlyPlus = 100 * sum(D2_02 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_02)),
  pctD2_03 = 100 * sum(D2_02 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_03)),
  pctD2_03_MonthlyPlus = 100 * sum(D2_03 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_03)),
  pctD2_04 = 100 * sum(D2_04 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_04)),
  pctD2_04_MonthlyPlus = 100 * sum(D2_04 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_04)),
  pctD2_05 = 100 * sum(D2_05 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_05)),
  pctD2_05_MonthlyPlus = 100 * sum(D2_05 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_05)),
  pctD2_06 = 100 * sum(D2_06 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_06)),
  pctD2_06_MonthlyPlus = 100 * sum(D2_06 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$D2_06)),
  pctD3_ever = 100 * sum(D3 %in% c("1", "2"), na.rm = T) / sum(!is.na(.SD$D3)),
  pctD3_12m = 100 * sum(D3 %in% c("2"), na.rm = T) / sum(!is.na(.SD$D3)),
  pctD4_ever = 100 * sum(D4 %in% c("1", "2"), na.rm = T) / sum(!is.na(.SD$D4)),
  pctD4_12m = 100 * sum(D4 %in% c("2"), na.rm = T) / sum(!is.na(.SD$D4)),
  D234_N = sum(!is.na(.SD$D4)),
  pctG4_01 = 100 * sum(G4_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_01)),
  pctG4_01_MonthlyPlus = 100 * sum(G4_01 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_01)),
  pctG4_02 = 100 * sum(G4_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_02)),
  pctG4_02_MonthlyPlus = 100 * sum(G4_02 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_02)),
  pctG4_03 = 100 * sum(G4_02 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_03)),
  pctG4_03_MonthlyPlus = 100 * sum(G4_03 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_03)),
  pctG4_04 = 100 * sum(G4_04 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_04)),
  pctG4_04_MonthlyPlus = 100 * sum(G4_04 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_04)),
  pctG4_05 = 100 * sum(G4_05 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_05)),
  pctG4_05_MonthlyPlus = 100 * sum(G4_05 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_05)),
  pctG4_06 = 100 * sum(G4_06 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_06)),
  pctG4_06_MonthlyPlus = 100 * sum(G4_06 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_06)),
  pctG4_07 = 100 * sum(G4_07 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_07)),
  pctG4_07_MonthlyPlus = 100 * sum(G4_07 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_07)),
  pctG4_08 = 100 * sum(G4_08 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_08)),
  pctG4_08_MonthlyPlus = 100 * sum(G4_08 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_08)),
  pctG4_09 = 100 * sum(G4_09 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_09)),
  pctG4_09_MonthlyPlus = 100 * sum(G4_09 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_09)),
  pctG4_10 = 100 * sum(G4_10 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_10)),
  pctG4_10_MonthlyPlus = 100 * sum(G4_10 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_10)),
  pctG4_11 = 100 * sum(G4_11 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_11)),
  pctG4_11_MonthlyPlus = 100 * sum(G4_11 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$G4_11)),
  G4_N = sum(!is.na(.SD$G4_01)),
  pctJ8_01 = 100 * sum(J8_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_01)),
  pctJ8_01_ModeratelyPlus = 100 * sum(J8_01 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_01)),
  pctJ8_02 = 100 * sum(J8_01 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_02)),
  pctJ8_02_ModeratelyPlus = 100 * sum(J8_02 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_02)),
  pctJ8_03 = 100 * sum(J8_02 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_03)),
  pctJ8_03_ModeratelyPlus = 100 * sum(J8_03 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_03)),
  pctJ8_04 = 100 * sum(J8_04 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_04)),
  pctJ8_04_ModeratelyPlus = 100 * sum(J8_04 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_04)),
  pctJ8_05 = 100 * sum(J8_05 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_05)),
  pctJ8_05_ModeratelyPlus = 100 * sum(J8_05 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_05)),
  pctJ8_06 = 100 * sum(J8_06 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_06)),
  pctJ8_06_ModeratelyPlus = 100 * sum(J8_06 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_06)),
  pctJ8_07 = 100 * sum(J8_07 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_07)),
  pctJ8_07_ModeratelyPlus = 100 * sum(J8_07 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_07)),
  pctJ8_08 = 100 * sum(J8_08 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_08)),
  pctJ8_08_ModeratelyPlus = 100 * sum(J8_08 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_08)),
  pctJ8_09 = 100 * sum(J8_09 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_09)),
  pctJ8_09_ModeratelyPlus = 100 * sum(J8_09 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_09)),
  pctJ8_10 = 100 * sum(J8_10 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_10)),
  pctJ8_10_ModeratelyPlus = 100 * sum(J8_10 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_10)),
  pctJ8_11 = 100 * sum(J8_11 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_11)),
  pctJ8_11_ModeratelyPlus = 100 * sum(J8_11 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_11)),
  pctJ8_12 = 100 * sum(J8_12 %in% c("1", "2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_12)),
  pctJ8_12_ModeratelyPlus = 100 * sum(J8_12 %in% c("2", "3", "4"), na.rm = T) / sum(!is.na(.SD$J8_12)),
  J8_N = sum(!is.na(.SD$J8_01))
), by = c("District", "SchoolYear")]
      
      write.xlsx(
        SUMMARY,
        paste0("CUSP_SUMMARY_DISTRICT_", current_date, ".xlsx"),
        quote = FALSE,
        na = "",
        rowNames = FALSE
      )
      
      return(summary)
    }, error = function(e) {
      message("Error generating site summary: ", e$message)
      return(data.table())
    })
    
    
    message("Processing complete!")
    return(Qs)
    
  }, error = function(e) {
    message("Error in process_questionnaires: ", e$message)
    return(Qs)
  })
}
