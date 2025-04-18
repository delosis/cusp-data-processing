rm()

# Source helper functions and load required libraries
tryCatch({

  source("CuspDownloadFunctions.R")
  source("CuspMatchingFunctions.R")
  source("CuspTaskFunctions.R")
  library(data.table)
  library(openxlsx)
  library(stringdist)
}, error = function(e) {
  stop("Failed to load required libraries and source files: ", e$message)
})


message("Loading manual QC data...")
# Load manual QC data
tryCatch({
  discardIDs <- read.xlsx("discardIDs.xlsx", colNames = FALSE)
  manualQCexclude <- read.xlsx('ManualQC.xlsx','Exclude')
  manualQCduplicates <- read.xlsx('ManualQC.xlsx','DuplicatesMatched')
  manualQCduplicatesRid <- read.xlsx('ManualQC.xlsx','DuplicatesMatchedTid')
  manualQCexcludeRid <- read.xlsx('ManualQC.xlsx','ExcludeTid')
  manualQCMatches <- read.xlsx('ManualQC.xlsx','Suggested TP')
}, error = function(e) {
  stop("Failed to load manual QC data: ", e$message)
})

# Check for registration checkpoint
if (file.exists("checkpoint_registration.RData")) {
  message("Loading registration checkpoint...")
  load("checkpoint_registration.RData")
} else {
  message("No registration checkpoint found. Starting registration data download...")
  
  # Registration IDs for different sites
  registrationSID <- c(536272, 429629, 635776, 113868, 286976, 179195, 618621)
  names(registrationSID) <- c(
    "CUSP_UBCO",
    "OPfS_FOUNDRY", 
    "CUSP_ONTARIO",
    "OPfS_UBCO",
    "OPfS_Dilico",
    "OPfS_Montreal",
    "CUSP_NS"
  )

  # Initialize registration data frame
  registrationDF <- NULL

  message("Starting registration data download...")
  ## GRAB THE REGISTRATION DATA FOR ALL SAMPLES
  for (i in 1:length(registrationSID)) {
    message(sprintf("Downloading registration data for %s...", names(registrationSID)[i]))
    df <- downloadSurveyData(registrationSID[[i]])
    setDT(df)
    df$RegSample <- names(registrationSID)[i]
    if (!is.null(registrationDF)) {
      registrationDF <- rbind(df, registrationDF, fill = TRUE)
    } else {
      registrationDF <- df
    }
  }



  # Remove incomplete attempts
  registrationDF <- registrationDF[registrationDF$psytoolsUserCode != "" |
                                   (registrationDF$psytools.username. != '' &
                                      !is.na(registrationDF$psytools.username.)), ]

  # Homogenize user codes
  registrationDF$psytoolsUserCode[registrationDF$psytoolsUserCode == "" |
                                   is.na(registrationDF$psytoolsUserCode)] <-
    registrationDF$psytools.username.[registrationDF$psytoolsUserCode == "" |
                                       is.na(registrationDF$psytoolsUserCode)]

  # Remove abandoned registrations
  registrationDF <- registrationDF[!is.na(registrationDF$psytoolsUserCode) &
                                    registrationDF$psytoolsUserCode != "" &
                                    registrationDF$submitdate != "", ]

  # Use submitdate for date
  registrationDF$date <- registrationDF$submitdate

  # Select relevant columns
  registrationDF <- registrationDF[, c(
    "id",
    "psytoolsUserCode",
    "RegSample",
    "dataTag",
    "consent",
    "date",
    "name",
    "school",
    "schoolID",
    "infolink",
    "contactConsent.future.",
    "contactConsent.results."
  )]

  # Rename user code column
  names(registrationDF)[names(registrationDF) == "psytoolsUserCode"] <- "User.code"

  # Convert date to Date type
  registrationDF$date <- as.Date(registrationDF$date)

  message("Registration data download complete. Saving checkpoint...")
  
  save(registrationDF, file = "checkpoint_registration.RData")
}

# Check for questionnaires checkpoint
if (file.exists("checkpoint_questionnaires.RData")) {
  message("Loading questionnaires checkpoint...")
  load("checkpoint_questionnaires.RData")
} else {
  message("No questionnaires checkpoint found. Starting Q1 data download and processing...")
  # Download and process Q1 data
  Q1 <- selectIteration(
    downloadSingleDataFile("CUSP_Q1-DIGEST_BASIC_SAMPLE_SQL"),
    valid = FALSE
  )
  setDT(Q1)

  Q1 <- rbind(Q1,
              selectIteration(
                downloadSingleDataFile("CUSP_Q1_SHORT-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE
              ),
              fill = T)

  Q1 <- Q1[User.code != 'DEV', ]

  message("Q1 data processing complete. Starting Q2 data download and processing...")
  # Download and process Q2 data
  Q2 <- selectIteration(
    downloadSingleDataFile("CUSP_Q2-DIGEST_BASIC_SAMPLE_SQL"),
    valid = FALSE
  )
  setDT(Q2)

  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_ONTARIO_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE
              ),
              fill = T)

  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_UBCO_DELTA_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE
              ),
              fill = T)

  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_OPfS_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE
              ),
              fill = T)

  Q2 <- Q2[User.code != 'DEV', ]

    message("Q2 data processing complete. Saving Q1 and Q2 checkpoint...")
    save(Q1, Q2, file = "checkpoint_questionnaires.RData")
}

# Clean Q1 and Q2 data
Q1 <- Q1[!duplicated(subset(Q1, select = c(User.code, Iteration, Trial)), fromLast = T), ]
Q1 <- Q1[Q1$Trial.result != 'skip_back', ]
Q1 <- Q1[Q1$Block != 'js', ]

Q2 <- Q2[!duplicated(subset(Q2, select = c(User.code, Iteration, Trial)), fromLast = T), ]
Q2 <- Q2[Q2$Trial.result != 'skip_back', ]
Q2 <- Q2[Q2$Block != 'js', ]

# Convert trial results to numeric
Q1$Trial.result <- as.numeric(Q1$Trial.result)

samples <- Q1[, c("User.code", "Sample(s)")]
samples <- rbind(samples, Q2[, c("User.code", "Sample(s)")])
samples <- unique(samples)
names(samples) <- c("User.code", "Sample")

message("Processing SURPS data and merging datasets...")
# Process SURPS data
SURPSmatch <- rotateQuestionnaire(Q1, idVar = c("User.code", "Completed.Timestamp"))
Q2match <- rotateQuestionnaire(Q2, idVar = c("User.code"))
setDT(SURPSmatch)
setDT(Q2match)

# Merge data
SURPSmatch <- samples[SURPSmatch, on = c(User.code = "User.code")]
QsMatch <- registrationDF[SURPSmatch, on = c(User.code = "User.code")]
matching <- QsMatch[Q2match, on = c(User.code = "User.code")]


message("Initial data processing complete. Saving pre-matching checkpoint...")
save(matching, file = "checkpoint_pre_matching.RData")

# Remove discarded IDs
matching <- matching[!User.code %in% discardIDs$X1,]

# Add testing date
matching$date_of_testing <- as.Date(matching$Processed.Timestamp)

# Select relevant columns
matching <- matching[, .(
  User.code,
  id,
  name,
  school,
  schoolID,
  dataTag,
  RegSample,
  Sample,
  B1,
  B5,
  date,
  date_of_testing,
  Completed.Timestamp
)]

# Add registration flag
matching$WasRegistered <- !is.na(matching$RegSample)
matching[is.na(matching$RegSample)]$RegSample <-
  matching[is.na(matching$RegSample)]$Sample

# Save original matching data
matching_orig <- matching

# Filter for CUSP samples
matching <- matching_orig[grepl('CUSP',RegSample), ]

# Remove test accounts
matching <- matching[!grepl('test',RegSample, ignore.case = TRUE),]
matching <- matching[!grepl('test',User.code, ignore.case = TRUE),]
matching <- matching[!grepl('test',name, ignore.case = TRUE),]
matching <- matching[!grepl('test',school, ignore.case = TRUE),]
matching <- matching[!grepl('test',schoolID, ignore.case = TRUE),]
matching <- matching[!grepl('test',dataTag, ignore.case = TRUE),]

# Remove manually excluded records
matching <- matching[!User.code %in% manualQCexclude$User.code]

setkey(matching, id, RegSample)
setDT(manualQCexcludeRid)
setkey(manualQCexcludeRid, rid, regSample)
matching <- matching[!manualQCexcludeRid]

# Set data tags for NS samples
matching[date_of_testing<'2023-01-01' & RegSample=='CUSP_NS',"dataTag"] <- 'C1-T1'
matching[date_of_testing>'2023-01-01' & RegSample=='CUSP_NS' & is.na(dataTag),"dataTag"] <- 'C2-T1'
matching[RegSample=='CUSP_NS' & is.na(schoolID),schoolID := regmatches(User.code, regexpr("S\\d+CUSP", User.code))]

message("Processing manual QC duplicates...")
# Process duplicates
setDT(manualQCduplicates)
setDT(manualQCduplicatesRid)

manualQCduplicates <- melt(manualQCduplicates,
                          id.vars = "User.code",
                          na.rm = TRUE,
                          value.name = "ManualQC.SuggestedDuplicate")
manualQCduplicates$variable <- NULL

manualQCduplicatesRid <- melt(manualQCduplicatesRid,
                             id.vars = c("RegSample", "Rid"),
                             na.rm = TRUE,
                             value.name = "Linked.Rid")
manualQCduplicatesRid$variable <- NULL

# Merge RIDs
manualQCduplicatesRid <- merge(manualQCduplicatesRid,
                              matching[RegSample=="CUSP_UBCO",c("User.code", "id")],
                              by.x="Rid",
                              by.y="id")
manualQCduplicatesRid <- merge(manualQCduplicatesRid,
                              matching[RegSample=="CUSP_UBCO",c("User.code", "id")],
                              by.x="Linked.Rid",
                              by.y="id")
manualQCduplicatesRid <- manualQCduplicatesRid[,c("User.code.x", "User.code.y")]
names(manualQCduplicatesRid) <- c("User.code", "ManualQC.SuggestedDuplicate")

# Filter duplicates
manualQCduplicates <- manualQCduplicates[User.code %in% matching$User.code &
                                          ManualQC.SuggestedDuplicate %in% matching$User.code]

# Combine duplicates
manualQCduplicates <- rbind(manualQCduplicates, manualQCduplicatesRid)

# Merge duplicates with matching data
matching <- merge(matching, manualQCduplicates, by="User.code", all.x=TRUE)

# Check for autoduplicate checkpoint
if (file.exists("checkpoint_autoduplicate.RData")) {
  message("Loading autoduplicate checkpoint...")
  load("checkpoint_autoduplicate.RData")
} else {
  message("Starting/resuming AutoDuplicate matching...")
  tryCatch({
    # Find duplicates and matches
    total_rows <- nrow(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing AutoDuplicate matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "AutoDuplicate", get_matching(matching[i, ], -10, 10))
    }

    message("AutoDuplicate matching complete. Saving checkpoint...")
    save(matching, file = "checkpoint_autoduplicate.RData")
  }, error = function(e) {
    message("Error during AutoDuplicate matching: ", e$message)
    save(matching, file = "checkpoint_autoduplicate_error.RData")
    stop(e)
  })
}

# Check for Y2 matches checkpoint
if (file.exists("checkpoint_y2matches.RData")) {
  message("Loading Y2 matches checkpoint...")
  load("checkpoint_y2matches.RData")
} else {
  message("Starting/resuming Y2 matching...")
  tryCatch({
    total_rows <- nrow(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing Y2 matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "Y2AutoMatch", get_matching(matching[i, ], 150, 551))
    }

    message("Y2 matching complete. Saving checkpoint...")
    save(matching, file = "checkpoint_y2matches.RData")
  }, error = function(e) {
    message("Error during Y2 matching: ", e$message)
    save(matching, file = "checkpoint_y2matches_error.RData")
    stop(e)
  })
}

# Check for matching complete checkpoint
if (file.exists("checkpoint_matching_complete.RData")) {
  message("Loading matching complete checkpoint...")
  load("checkpoint_matching_complete.RData")
} else {
  message("Starting/resuming Y3 matching...")
  tryCatch({
    total_rows <- nrow(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing Y3 matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "Y3AutoMatch", get_matching(matching[i, ], 550, 950))
    }

    message("All matching complete! Saving final matching checkpoint...")
    save(matching, file = "checkpoint_matching_complete.RData")
  }, error = function(e) {
    message("Error during Y3 matching: ", e$message)
    save(matching, file = "checkpoint_matching_complete_error.RData")
    stop(e)
  })
}

# Check for resolution checkpoint
if (file.exists("checkpoint_resolution.RData")) {
  message("Loading resolution checkpoint...")
  load("checkpoint_resolution.RData")
} else {
  message("Starting/resuming duplicate resolution...")
  tryCatch({
    # Resolve duplicates and get mapping
    resolution_result <- resolve_duplicates(matching)
    matching <- resolution_result$resolved
    duplicate_map <- resolution_result$duplicate_map

    message("Duplicate resolution complete. Saving checkpoint...")
    save(matching, duplicate_map, file = "checkpoint_resolution.RData")
  }, error = function(e) {
    message("Error during duplicate resolution: ", e$message)
    save(matching, file = "checkpoint_resolution_error.RData")
    stop(e)
  })
}

message("Generating output files...")

# Add District column
matching$District <- matching$RegSample

matching$District[matching$schoolID %in% c('ndss', 'ss', 'sa', 'sdss', 'dl', 'dss', 'bu') &
                   (matching$RegSample == "CUSP_UBCO" |
                      matching$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_DELTA"

matching$District[matching$schoolID %in% c('wss', 'skss', 'vss') &
                   (matching$RegSample == "CUSP_UBCO" |
                      matching$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_KAMLOOPS"

# Save results by region
current_date <- format(Sys.Date(), "%Y-%m-%d")



message("Processing questionnaires with duplicate resolution...")
# Process questionnaires with duplicate resolution

 # Add District column to registration DF to it is accessible to processed Qs
registrationDF$District <- registrationDF$RegSample

registrationDF$District[registrationDF$schoolID %in% c('ndss', 'ss', 'sa', 'sdss', 'dl', 'dss', 'bu') &
              (registrationDF$RegSample == "CUSP_UBCO" |
                registrationDF$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_DELTA"

registrationDF$District[registrationDF$schoolID %in% c('wss', 'skss', 'vss') &
              (registrationDF$RegSample == "CUSP_UBCO" |
                registrationDF$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_KAMLOOPS"

# This also saves the output files for each region
questionnaires <- process_questionnaires(duplicate_map, Q1, Q2, current_date)

# Calculate QC flags from questionnaire data
message("Calculating QC flags from questionnaire data...")
qc_flags <- QCflagQdata(questionnaires)

# Process each region's data before saving
message("Anonymizing school IDs and filtering columns...")
matching_anon <- process_and_anonymize(matching)

# Add QC flags to matching data
message("Adding QC flags to matching data...")
matching_anon[qc_flags, on = "User.code", names(qc_flags)[-1] := mget(paste0("i.", names(qc_flags)[-1]))]

ns_data <- matching_anon[RegSample == "CUSP_NS", ]
ubco_data <- matching_anon[RegSample == "CUSP_UBCO", ]
ontario_data <- matching_anon[RegSample == "CUSP_ONTARIO", ]

write.xlsx(
  ns_data,
  paste0("CUSP_MATCHING_NS_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

write.xlsx(
  ubco_data,
  paste0("CUSP_MATCHING_UBCO_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

write.xlsx(
  ontario_data,
  paste0("CUSP_MATCHING_ON_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

message("Generating additional Matching QC files...")
# Generate additional output files
# 1. Nameless matches
nameless_matches <- get_nameless_matches(matching)
write.xlsx(
  nameless_matches,
  paste0("CUSP_NAMELESS_MATCHES_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

# 2. Date discrepancies (matches more than 30 days from expected year intervals)
date_discrepancies <- get_date_discrepancies(matching, tolerance_days = 30)
write.xlsx(
  date_discrepancies,
  paste0("CUSP_DATE_DISCREPANCIES_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

# Task Data Processing
message("Downloading and processing task data...")

# Download PALP data
message("Downloading PALP data...")
palp <- selectIteration(
  downloadSingleDataFile("CUSP_PALP-BASIC_DIGEST"),
  valid = FALSE
)
setDT(palp)

# Download and combine CMS data
message("Downloading and combining CMS data...")
cms <- selectIteration(
  downloadSingleDataFile("CUSP_CMS2-BASIC_DIGEST"),
  valid = FALSE
)
cms <- rbind(
  cms,
  selectIteration(
    downloadSingleDataFile("CUSP_CMS2R-BASIC_DIGEST"),
    valid = FALSE
  ),
  fill = TRUE
)
setDT(cms)

# Download SWM data
message("Downloading SWM data...")
swm <- selectIteration(
  downloadSingleDataFile("CUSP_SWM10-SWM_DIGEST"),
  valid = FALSE
)
setDT(swm)

# Download CFT data
message("Downloading CFT data...")
cft <- selectIteration(
  downloadSingleDataFile("CUSP_CFT-BASIC_DIGEST"),
  valid = FALSE
)
setDT(cft)

message("Task data download complete!")

# Process task data
message("Processing task data...")

# Process PALP data
palp_summary <- summaryVarsPALP(palp)
write.xlsx(palp_summary, "CUSP_PALP_SUMMARY.xlsx", quote = FALSE, na = "", rowNames = FALSE)

# Process CMS data
cms_summary <- summaryVarsCMS(cms)
write.xlsx(cms_summary, "CUSP_CMS_SUMMARY.xlsx", quote = FALSE, na = "", rowNames = FALSE)

# Process CFT data - merge in age data first
cft_with_age <- merge(cft, matching_anon[, .(User.code, AgeAtTesting = age_at_testing)], by = "User.code", all.x = TRUE)
cft_summary <- summaryVarsCFT(cft_with_age)
write.xlsx(cft_summary, "CUSP_CFT_SUMMARY.xlsx", quote = FALSE, na = "", rowNames = FALSE)

# Process SWM data
swm_summary <- summaryVarsSWM(swm)
write.xlsx(swm_summary, "CUSP_SWM_SUMMARY.xlsx", quote = FALSE, na = "", rowNames = FALSE)

message("Task data processing complete and summary files saved.")
