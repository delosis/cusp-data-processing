rm()

# Source helper functions and load required libraries
tryCatch({
  source("CuspDownloadFunctions.R")
  source("CuspMatchingFunctions.R")
  source("CuspTaskFunctions.R")
  source("CuspStatisticsFunctions.R")
  source("CuspReportGeneration.R")
  library(data.table)
  library(openxlsx)
  library(stringdist)
}, error = function(e) {
  stop("Failed to load required libraries and source files: ", e$message)
})

# Initialize statistics collection
message("Initializing statistics collection...")
run_stats <- init_statistics()


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
  # Calculate registration statistics even when loaded from checkpoint
  message("Calculating registration statistics from checkpoint...")
  run_stats <- calculate_registration_stats(registrationDF, run_stats)
  run_stats <- calculate_checkpoint_stats(registrationDF, "After_Registration_Checkpoint", run_stats)
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
  
  # find registrations under the non-atomic registration endpoint ( covering a single session in OPfS - 8 registrations)
  unusableUserCodes = registrationDF$User.code[duplicated(registrationDF$User.code)]
  registrationDF<-registrationDF[!User.code %in% unusableUserCodes,]

  # Convert date to Date type
  registrationDF$date <- as.Date(registrationDF$date)

  message("Registration data download complete. Saving checkpoint...")
  
  save(registrationDF, file = "checkpoint_registration.RData")
}

# Calculate registration statistics
message("Calculating registration statistics...")
run_stats <- calculate_registration_stats(registrationDF, run_stats)
run_stats <- calculate_checkpoint_stats(registrationDF, "After_Registration", run_stats)

# Check for questionnaires checkpoint
if (file.exists("checkpoint_questionnaires.RData")) {
  message("Loading questionnaires checkpoint...")
  load("checkpoint_questionnaires.RData")
  # Calculate questionnaire statistics even when loaded from checkpoint
  message("Calculating questionnaire statistics from checkpoint...")
  run_stats <- calculate_checkpoint_stats(Q1, "After_Q1_Load_Checkpoint", run_stats)
  run_stats <- calculate_checkpoint_stats(Q2, "After_Q2_Load_Checkpoint", run_stats)
} else {
  message("No questionnaires checkpoint found. Starting Q1 data download and processing...")
  # Download and process Q1 data
  Q1 <- selectIteration(
    downloadSingleDataFile("CUSP_Q1-DIGEST_BASIC_SAMPLE_SQL"),
    valid = FALSE,
    completed = FALSE,
    isQuestionnaire = FALSE
  )
  setDT(Q1)

  Q1 <- rbind(Q1,
              selectIteration(
                downloadSingleDataFile("CUSP_Q1_SHORT-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE,
                completed = FALSE,
                isQuestionnaire = FALSE
              ),
              fill = T)

  Q1 <- Q1[User.code != 'DEV' & !User.code %in% unusableUserCodes, ]

  message("Q1 data processing complete. Starting Q2 data download and processing...")
  # Download and process Q2 data
  Q2 <- selectIteration(
    downloadSingleDataFile("CUSP_Q2-DIGEST_BASIC_SAMPLE_SQL"),
    valid = FALSE,
    completed = FALSE,
    isQuestionnaire = FALSE
  )


  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_ONTARIO_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE,
                completed = FALSE,
                isQuestionnaire = FALSE
              ),
              fill = T)

  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_UBCO_DELTA_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE,
                completed = FALSE,
                isQuestionnaire = FALSE
              ),
              fill = T)

  Q2 <- rbind(Q2,
              selectIteration(
                downloadSingleDataFile("CUSP_OPfS_Q2-DIGEST_BASIC_SAMPLE_SQL"),
                valid = FALSE,
                completed = FALSE,
                isQuestionnaire = FALSE
              ),
              fill = T)
  setDT(Q2)
  Q2 <- Q2[User.code != 'DEV' & !User.code %in% unusableUserCodes, ]

    message("Q2 data processing complete. Saving Q1 and Q2 checkpoint...")
    save(Q1, Q2, file = "checkpoint_questionnaires.RData")
}

# Calculate questionnaire statistics
message("Calculating questionnaire statistics...")
run_stats <- calculate_checkpoint_stats(Q1, "After_Q1_Load", run_stats)
run_stats <- calculate_checkpoint_stats(Q2, "After_Q2_Load", run_stats)

# Clean Q1 and Q2 data

Q1 <- Q1[Q1$Trial.result != 'skip_back', ]
Q1 <- Q1[Q1$Trial.result != 'not_shown_back', ]
Q1 <- Q1[Q1$Block != 'js', ]
Q1 <- Q1[!duplicated(subset(Q1, select = c(User.code, Trial)), fromLast = T), ]

Q2 <- Q2[Q2$Trial.result != 'skip_back', ]
Q2 <- Q2[Q2$Trial.result != 'not_shown_back', ]
Q2 <- Q2[Q2$Block != 'js', ]
Q2 <- Q2[!duplicated(subset(Q2, select = c(User.code, Trial)), fromLast = T), ]


# Convert trial results to numeric
Q1$Trial.result <- as.numeric(Q1$Trial.result)

samples <- Q1[, c("User.code", "Sample(s)")]
samples <- rbind(samples, Q2[, c("User.code", "Sample(s)")])
samples <- unique(samples)
names(samples) <- c("User.code", "Sample")

message("Processing SURPS data and merging datasets...")
# Process SURPS data
SURPSmatch <- rotateQuestionnaire(Q1, idVar = c("User.code", "Completed.Timestamp", "Processed.Timestamp"))
Q2match <- rotateQuestionnaire(Q2, idVar = c("User.code"))
setDT(SURPSmatch)
setDT(Q2match)

# Merge data - CHANGED TO BRING IN RECORDS WITH NO Q2
SURPSmatch <- samples[SURPSmatch, on = c(User.code = "User.code")]
QsMatch <- registrationDF[SURPSmatch, on = c(User.code = "User.code")]
matching <- QsMatch[Q2match, on = c(User.code = "User.code"), nomatch = NA]


message("Initial data processing complete. Saving pre-matching checkpoint...")
save(matching, file = "checkpoint_pre_matching.RData")

# Calculate statistics before exclusions (but after OPfS removal for reporting)
# Filter OPfS first for reporting purposes
matching_before_opfs <- matching
matching_cusp_only <- matching[grepl('CUSP', RegSample, ignore.case = TRUE), ]
run_stats$exclusions$opfs_removed <- nrow(matching_before_opfs) - nrow(matching_cusp_only)

# Calculate questionnaire linkage on CUSP-only data
run_stats <- calculate_questionnaire_linkage_stats(matching_cusp_only, Q1, Q2, run_stats)
run_stats <- calculate_checkpoint_stats(matching_cusp_only, "After_Initial_Merge", run_stats)

# Track exclusions on CUSP data
matching_before_discard <- matching_cusp_only
# Remove discarded IDs
matching_cusp_only <- matching_cusp_only[!User.code %in% discardIDs$X1,]
run_stats$exclusions$discardIDs <- nrow(matching_before_discard) - nrow(matching_cusp_only)
run_stats <- calculate_checkpoint_stats(matching_cusp_only, "After_DiscardIDs", run_stats)

# Continue with full matching for processing, but use CUSP-only for stats
matching <- matching_cusp_only

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

# OPfS already removed above, continue with CUSP-only data

# Track test account exclusion
matching_before_test <- matching
# Remove test accounts
matching <- matching[!grepl('test',RegSample, ignore.case = TRUE),]
matching <- matching[!grepl('test',User.code, ignore.case = TRUE),]
matching <- matching[!grepl('test',name, ignore.case = TRUE),]
matching <- matching[!grepl('test',school, ignore.case = TRUE),]
matching <- matching[!grepl('test',schoolID, ignore.case = TRUE),]
matching <- matching[!grepl('test',dataTag, ignore.case = TRUE),]
run_stats$exclusions$test_accounts <- nrow(matching_before_test) - nrow(matching)
run_stats <- calculate_checkpoint_stats(matching, "After_Test_Removal", run_stats)

# Track manual QC exclusions
matching_before_manualQC <- matching
# Remove manually excluded records
matching <- matching[!User.code %in% manualQCexclude$User.code]
run_stats$exclusions$manualQC_exclude_usercode <- nrow(matching_before_manualQC) - nrow(matching)

setkey(matching, id, RegSample)
setDT(manualQCexcludeRid)
setkey(manualQCexcludeRid, rid, regSample)
matching_before_manualQC_rid <- matching
matching <- matching[!manualQCexcludeRid]
run_stats$exclusions$manualQC_exclude_rid <- nrow(matching_before_manualQC_rid) - nrow(matching)
run_stats$exclusions$total_excluded <- run_stats$exclusions$discardIDs + 
                                       run_stats$exclusions$opfs_removed + 
                                       run_stats$exclusions$test_accounts + 
                                       run_stats$exclusions$manualQC_exclude_usercode + 
                                       run_stats$exclusions$manualQC_exclude_rid
run_stats <- calculate_checkpoint_stats(matching, "After_ManualQC_Exclusions", run_stats)

# Set data tags for NS samples
matching[date_of_testing<'2023-01-01' & RegSample=='CUSP_NS',"dataTag"] <- 'c1-t1'
matching[date_of_testing>'2023-01-01' & RegSample=='CUSP_NS' & is.na(dataTag),"dataTag"] <- 'c2-t1'
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
run_stats <- calculate_checkpoint_stats(matching, "After_ManualQC_Duplicates", run_stats)

# Check for autoduplicate checkpoint
if (file.exists("checkpoint_autoduplicate.RData")) {
  message("Loading autoduplicate checkpoint...")
  load("checkpoint_autoduplicate.RData")
} else {
  message("Starting/resuming AutoDuplicate matching...")
  tryCatch({
    # Find duplicates and matches
    total_rows <- nrow(matching)
    setDT(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing AutoDuplicate matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "AutoDuplicate", get_matching(matching, matching[i, ], "duplicate"))
    }

    message("AutoDuplicate matching complete. Saving checkpoint...")
    save(matching, file = "checkpoint_autoduplicate.RData")
    
    run_stats <- calculate_checkpoint_stats(matching, "After_AutoDuplicate", run_stats)
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
    setDT(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing Y2 matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "Y2AutoMatch", get_matching(matching, matching[i, ],"tp1"))
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
    setDT(matching)
    for (i in 1:total_rows) {
      if(i %% 100 == 0) {
        message(sprintf("Processing Y3 matches: row %d of %d (%.1f%%)", 
                       i, total_rows, i/total_rows*100))
      }
      set(matching, i, "Y3AutoMatch", get_matching(matching, matching[i, ], "tp2"))
    }

    message("All matching complete! Saving final matching checkpoint...")
    save(matching, file = "checkpoint_matching_complete.RData")
    
    # Calculate matching statistics
    run_stats <- calculate_matching_stats(matching, run_stats)
    run_stats <- check_manualQC_matches(matching, manualQCMatches, run_stats)
    run_stats <- calculate_checkpoint_stats(matching, "After_All_Matching", run_stats)
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
  # Calculate duplicate stats from duplicate_map even when loaded from checkpoint
  run_stats <- calculate_duplicate_stats(duplicate_map, matching, run_stats)
} else {
  message("Starting/resuming duplicate resolution...")
  tryCatch({
    # Resolve duplicates and get mapping
  setDT(matching)
    resolution_result <- resolve_duplicates(matching)
    matching <- resolution_result$resolved
    duplicate_map <- resolution_result$duplicate_map
    message("Duplicate resolution complete. Saving checkpoint...")
    save(matching, duplicate_map, file = "checkpoint_resolution.RData")
    
    # Update statistics after duplicate resolution
    run_stats <- calculate_checkpoint_stats(matching, "After_Duplicate_Resolution", run_stats)
    # Calculate duplicate stats from duplicate_map
    run_stats <- calculate_duplicate_stats(duplicate_map, matching, run_stats)
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

# Process questionnaires
message("Processing questionnaires...")
questionnaires <- process_questionnaires(duplicate_map, Q1, Q2, current_date, qc_flags = NULL, save_files = FALSE)

# Calculate QC flags from questionnaire data
message("Calculating QC flags from questionnaire data...")
qc_flags <- QCflagQdata(questionnaires)


# Process each region's data before saving
message("Anonymizing school IDs and filtering columns...")
matching_anon <- process_and_anonymize(matching)

# Add QC flags to matching data
message("Adding QC flags to matching data...")
matching_anon[qc_flags, on = "User.code", names(qc_flags)[-1] := mget(paste0("i.", names(qc_flags)[-1]))]

# Create helper dataset with User.code, dataTag, and QC flags for merging into all files
user_metadata <- matching[, .(User.code, dataTag)]
user_metadata <- merge(user_metadata, qc_flags, by = "User.code", all.x = TRUE)

ns_data <- matching_anon[RegSample == "CUSP_NS", ]
ubco_data <- matching_anon[RegSample == "CUSP_UBCO", ]
ontario_data <- matching_anon[RegSample == "CUSP_ONTARIO", ]

# Add QC flags and ensure dataTag is in PII files
ns_data_pis <- matching[RegSample == "CUSP_NS", ]
ubco_data_pis <- matching[RegSample == "CUSP_UBCO", ]
ontario_data_pis <- matching[RegSample == "CUSP_ONTARIO", ]



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
write.xlsx(
  ns_data_pis,
  paste0("CUSP_MATCHING_NS_PII_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

write.xlsx(
  ubco_data_pis,
  paste0("CUSP_MATCHING_PII_UBCO_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)

write.xlsx(
  ontario_data_pis,
  paste0("CUSP_MATCHING_PII_ON_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)


# Merge QC flags into questionnaires and save
message("Saving questionnaires with QC flags...")
questionnaires <- merge(questionnaires, qc_flags, by = "User.code", all.x = TRUE)
sites <- c("CUSP_NS", "CUSP_UBCO", "CUSP_ONTARIO")
for (site in sites) {
  tryCatch({
    site_data <- questionnaires[RegSample == site, -c("RegSample", "name", "District"), with = FALSE]
    write.xlsx(
      site_data,
      paste0("CUSP_QUESTIONNAIRES_", site, "_", current_date, ".xlsx"),
      quote = FALSE,
      na = "",
      rowNames = FALSE
    )
  }, error = function(e) {
    message("Error saving questionnaire file for ", site, ": ", e$message)
  })
}


message("Generating additional Matching QC files...")
# Generate additional output files
# 1. Nameless matches
nameless_matches <- get_nameless_matches(matching)
run_stats <- calculate_nameless_stats(nameless_matches, matching, run_stats)
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
palp_raw <- downloadSingleDataFile("CUSP_PALP-BASIC_DIGEST")
setDT(palp_raw)

# Download and combine CMS data
message("Downloading and combining CMS data...")
cms_raw <- downloadSingleDataFile("CUSP_CMS2-BASIC_DIGEST")
setDT(cms_raw)
cmsr_raw <-downloadSingleDataFile("CUSP_CMS2R-BASIC_DIGEST")
setDT(cmsr_raw)

# Download SWM data
message("Downloading SWM data...")
swm_raw <- downloadSingleDataFile("CUSP_SWM10-SWM_DIGEST")
setDT(swm_raw)

# Download CFT data
message("Downloading CFT data...")
cft_raw <- downloadSingleDataFile("CUSP_CFT-BASIC_DIGEST")
setDT(cft_raw)

message("Task data download complete!")

# Apply duplicate mapping to raw task data
message("Applying duplicate mapping to task data...")
for (i in 1:nrow(duplicate_map)) {
  palp_raw[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
  cms_raw[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
  cmsr_raw[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
  swm_raw[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
  cft_raw[User.code == duplicate_map$removed_id[i], User.code := duplicate_map$kept_id[i]]
}



# Select iterations for each task
palp <- selectIteration(palp_raw, iterationFunction = min, completed = TRUE, allowIncomplete = TRUE, isQuestionnaire = FALSE)
cms <- rbind(
  selectIteration(cms_raw, iterationFunction = min, completed = TRUE, allowIncomplete = TRUE, isQuestionnaire = FALSE),
  selectIteration(cmsr_raw, iterationFunction = min, completed = TRUE, allowIncomplete = TRUE, isQuestionnaire = FALSE),
  fill=TRUE
)
swm <- selectIteration(swm_raw, iterationFunction = min, completed = TRUE, allowIncomplete = TRUE, isQuestionnaire = FALSE)
cft <- selectIteration(cft_raw, iterationFunction = min, completed = TRUE, allowIncomplete = TRUE, isQuestionnaire = FALSE)

# Process task data
message("Processing task data...")

# Process PALP data
# Add dataTag and QC flags to raw PALP data
palp <- merge(palp, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  palp,
  paste0("CUSP_PALP_RAW_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)
palp_summary <- summaryVarsPALP(palp)
# Add dataTag and QC flags to PALP summary
palp_summary <- merge(palp_summary, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  palp_summary,
  paste0("CUSP_PALP_SUMMARY_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)


# Process CMS data
# Add dataTag and QC flags to raw CMS data
cms <- merge(cms, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  cms,
  paste0("CUSP_CMS_RAW_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)
cms_summary <- summaryVarsCMS(cms)
# Add dataTag and QC flags to CMS summary
cms_summary <- merge(cms_summary, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  cms_summary,
  paste0("CUSP_CMS_SUMMARY_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)


# Process CFT data - merge in age data, dataTag, and QC flags
# Add dataTag and QC flags to raw CFT data
cft <- merge(cft, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  cft,
  paste0("CUSP_CFT_RAW_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)
cft_with_age <- merge(cft, matching_anon[, .(User.code, AgeAtTesting = age_at_testing)], by = "User.code", all.x = TRUE)
cft_summary <- summaryVarsCFT(cft_with_age)
# Add dataTag and QC flags to CFT summary (dataTag already in cft_with_age, but QC flags may not be)
cft_summary <- merge(cft_summary, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  cft_summary,
  paste0("CUSP_CFT_SUMMARY_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)


# Process SWM data
# Add dataTag and QC flags to raw SWM data
swm <- merge(swm, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  swm,
  paste0("CUSP_SWM_RAW_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)
swm_summary <- summaryVarsSWM(swm)
# Add dataTag and QC flags to SWM summary
swm_summary <- merge(swm_summary, user_metadata, by = "User.code", all.x = TRUE)
write.xlsx(
  swm_summary,
  paste0("CUSP_SWM_SUMMARY_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)


message("Task data processing complete and summary files saved.")

# Generate final statistics and report
message("Generating run statistics report...")
# Use matching (before anonymization) for final stats to preserve all information
run_stats <- calculate_checkpoint_stats(matching, "Final_Output", run_stats)
current_date <- format(Sys.Date(), "%Y-%m-%d")
report_file <- paste0("CUSP_Run_Statistics_", current_date, ".html")
generate_html_report(run_stats, report_file, current_date)
message(sprintf("Run statistics report saved to: %s", report_file))
