#' Task Summary Functions
#' Optimized versions using data.table for better performance
#' @import data.table



#' QCflagQdata
#' Adds quality control flags to questionnaire data
#' @param QData A data.table containing questionnaire data
#' @return A data.table containing only User.code and QC flags
#' @export
QCflagQdata <- function(QData) {
  # Convert to data.table if not already
  if (!inherits(QData, "data.table")) {
    setDT(QData)
  }
  
  # Create a copy to avoid modifying the original
  QData_copy <- copy(QData)
  
  # DEPAPO Foil QC - now using C2_W (SEMORON)
  QData_copy[, QC_DEPAPO_FOIL := as.integer(!is.na(C2_W) & C2_W > 0)]
  
  # DEPAPO Uncommon > 2.5 QC
  # TODO: Review this list of uncommon drugs for CUSP - the list has expanded significantly
  # Current list based on original DEPAPO uncommon drugs mapping to new CUSP variables
  uncommon_drugs <- c("C2_E",  # SPICE (K2, K3, etc.)
                      "C2_V",  # STEROIDS
                      "C2_H",  # STIMULANTS (Ritalin, Adderall, etc.)
                      "C2_I",  # SEDATIVES/TRANQUILLIZERS
                      "C2_K",  # COCAINE
                      "C2_O",  # HEROIN
                      "C2_S")  # LSD
  QData_copy[, (uncommon_drugs) := lapply(.SD, function(x) {
    x[x %in% c("DK", "REFUSE")] <- NA
    return(as.numeric(x))
  }), .SDcols = uncommon_drugs]
  
  QData_copy[, QC_DEPAPO_UNCOMMON2.5 := as.integer(rowMeans(.SD, na.rm = TRUE) > 2.5), .SDcols = uncommon_drugs]
  
  # SURPS One Answer QC - using A1_a to A1_w
  surps_cols <- grep("^A1_[a-w]$", names(QData_copy), value = TRUE)
  QData_copy[, (surps_cols) := lapply(.SD, function(x) {
    x[x %in% c("DK", "REFUSE")] <- NA
    return(as.numeric(x))
  }), .SDcols = surps_cols]
  
  QData_copy[, QC_SURPS_ONE_ANSWER := as.integer(
    fcase(
      # If all values are NA, not a single response
      all(is.na(.SD)), 0L,
      # If min equals max (excluding NAs), it's a single response
      min(.SD, na.rm = TRUE) == max(.SD, na.rm = TRUE), 1L,
      # Otherwise not a single response
      default = 0L
    )
  ), .SDcols = surps_cols]
  
  # SDQ One Answer QC - using J12_01 to J12_25
  sdq_cols <- grep("^J12_(0[1-9]|1[0-9]|2[0-5])$", names(QData_copy), value = TRUE)
  QData_copy[, (sdq_cols) := lapply(.SD, function(x) {
    x[x %in% c("DK", "REFUSE")] <- NA
    return(as.numeric(x))
  }), .SDcols = sdq_cols]
  QData_copy[, QC_SDQ_ONE_ANSWER := as.integer(
    fcase(
      # If all values are NA, not a single response
      all(is.na(.SD)), 0L,
      # If min equals max (excluding NAs), it's a single response
      min(.SD, na.rm = TRUE) == max(.SD, na.rm = TRUE), 1L,
      # Otherwise not a single response
      default = 0L
    )
  ), .SDcols = sdq_cols]
  
  # Return only User.code and QC flags
  return(QData_copy[, .(User.code, QC_DEPAPO_FOIL, QC_DEPAPO_UNCOMMON2.5, QC_SURPS_ONE_ANSWER, QC_SDQ_ONE_ANSWER)])
}



#' Derive CUSP SURPS scores
#' @param df Data frame containing SURPS data
#' @param requiresReverseCoding Boolean indicating if reverse coding is needed
#' @return Data frame with derived scores
#' @export
deriveCUSPSURPS <- function(df, requiresReverseCoding = FALSE, requiresRotating = TRUE) {
  if (!inherits(df, "data.table")) {
    setDT(df)
  }
  
  if(requiresRotating) {
    df <- rotateQuestionnaire(df)
  }
  
  if (requiresReverseCoding) {
    # reverse code
    reverseVariables <- c('A1_a$', 'A1_d$', 'A1_g$', 'A1_m$', 'A1_t$','A1_w$')
    df <- recodeVariables(df, reverseVariables,  function(x) {5-x})
  }
  
  # Calculate means and sums using data.table syntax
  # Hopelessness
  h_cols <- grep("A1_aR?$|A1_dR?$|A1_gR?$|A1_mR?$|A1_q|A1_tR?$|A1_wR?$", names(df), value = TRUE)
  df[, `:=`(
    h_mean = rowMeans(.SD, na.rm = TRUE),
    h_sum = rowSums(.SD, na.rm = TRUE)
  ), .SDcols = h_cols]
  
  # Anxiety Sensitivity
  as_cols <- grep("A1_h|A1_j|A1_n|A1_r|A1_u", names(df), value = TRUE)
  df[, `:=`(
    as_mean = rowMeans(.SD, na.rm = TRUE),
    as_sum = rowSums(.SD, na.rm = TRUE)
  ), .SDcols = as_cols]
  
  # Impulsivity
  imp_cols <- grep("A1_b$|A1_e|A1_k|A1_o|A1_v", names(df), value = TRUE)
  df[, `:=`(
    imp_mean = rowMeans(.SD, na.rm = TRUE),
    imp_sum = rowSums(.SD, na.rm = TRUE)
  ), .SDcols = imp_cols]
  
  # Sensation Seeking
  ss_cols <- grep("A1_c$|A1_f|A1_i|A1_l|A1_p|A1_s", names(df), value = TRUE)
  df[, `:=`(
    ss_mean = rowMeans(.SD, na.rm = TRUE),
    ss_sum = rowSums(.SD, na.rm = TRUE)
  ), .SDcols = ss_cols]
  
  return(df)
}

#' PALP Summary data
#' Generates summary variables for PALP task using data.table
#' @param PALPData data.table containing the long format PALP data
#' @return data.table with summary variables
summaryVarsPALP <- function(PALPData) {
  if (!is.data.table(PALPData)) setDT(PALPData)
  
  # Create a copy for summary calculations
  PALP_summary <- copy(PALPData)
  
  # Calculate scores - properly handle all 4 conditions
  PALP_summary[, `:=`(
    Om = fifelse(!grepl("space", Response) & grepl("^I", Trial.result), 1L, 0L),
    Com = fifelse(grepl("space", Response) & grepl("^I", Trial.result), 1L, 0L),
    condition = fcase(
      grepl("PP", Block), "PP",
      grepl("PR", Block), "PR",
      grepl("RP", Block), "RP",
      grepl("RR", Block), "RR"
    )
  )]
  
  # Calculate QC flags per condition
  PALP_summary[, `:=`(
    # Worse than chance at 5% level
    PALP_QC_PALP_WORSE_THAN_CHANCE = fifelse(
      binom.test(sum(Om), nrow(.SD), p = 0.5)$p.value > 0.05 & 
      binom.test(sum(Com), nrow(.SD), p = 0.5)$p.value > 0.05,
      1L, 0L
    ),
    # Too fast - less than 200ms
    PALP_QC_PALP_TOO_FAST = fifelse(
      mean(Response.time..ms., na.rm = TRUE) < 200,
      1L, 0L
    )
  ), by = .(User.code, condition)]
  
  # Calculate summary statistics
  PALP_summary[, `:=`(
    # Count trials (excluding summary trials)
    Trials = sum(!grepl("summary", Trial)),
    # Raw counts
    Om_raw = sum(Om, na.rm = TRUE),
    Com_raw = sum(Com, na.rm = TRUE),
    # RT means
    RT_mean = mean(Response.time..ms., na.rm = TRUE),
    RT_Com_mean = mean(Response.time..ms.[Com == 1], na.rm = TRUE),
    RT_Hit_mean = mean(Response.time..ms.[Response == "space"], na.rm = TRUE),
    # Score from summary trials - extract from Trial.result where Trial contains "summary"
    Score = mean(as.numeric(Trial.result[grepl("summary", Trial)]), na.rm = TRUE)
  ), by = .(User.code, condition)]
  
  # Convert to proportions
  PALP_summary[, `:=`(
    Om = Om_raw / (Trials / 2),
    Com = Com_raw / (Trials / 2)
  )]
  
  # Convert to proportions
  PALP_summary[, `:=`(
    Hits = 1 - Om,
    CorrectNR = 1 - Com
  )]
  
  # Calculate aprime and bppd
  aprime <- function(hit, fa) {
    (0.5 + (hit - fa) * (1 + hit - fa) / (4 * hit * (1 - fa)))
  }
  
  bppd <- function(hit, fa) {
    ((1-hit)*(1-fa)-hit*fa) / ((1-hit)*(1-fa)+hit*fa)
  }
  
  PALP_summary[, `:=`(
    aprime = aprime(Hits, Com),
    bppd = bppd(Hits, Com)
  )]
  
  # Select and order columns
  PALP_summary <- PALP_summary[, .(
    User.code, condition,
    Om, Com, Hits, CorrectNR,
    RT_mean, RT_Com_mean, RT_Hit_mean,
    Score, aprime, bppd,
    PALP_QC_PALP_WORSE_THAN_CHANCE, PALP_QC_PALP_TOO_FAST
  )]
  
  # Remove practice trials (rows with no condition)
  PALP_summary <- PALP_summary[!is.na(condition)]
  
  # Take one row per condition (all values are the same)
  PALP_summary <- unique(PALP_summary)
  
  # Reshape to wide format
  PALP_summary <- dcast(
    melt(PALP_summary, id.vars = c("User.code", "condition")),
    User.code ~ condition + variable,
    value.var = "value"
  )
  
  return(PALP_summary)
}

#' CMS Summary data
#' Generates summary variables for CMS task using data.table
#' @param CMSData data.table containing the long format CMS data
#' @return data.table with summary variables
summaryVarsCMS <- function(CMSData) {
  if (!is.data.table(CMSData)) setDT(CMSData)
  
  # Create a copy for summary calculations
  CMS_summary <- copy(CMSData)
  
  # Split Trial.result into Score and Trial.result
  CMS_summary[, c("Score", "Trial.result") := tstrsplit(Trial.result, "|", fixed = TRUE)]
  
  # Select and order columns
  CMS_summary <- CMS_summary[, .(User.code, Trial, Score, Completed.Timestamp)]
  
  # Reshape to wide format for scores
  CMS_summary <- dcast(CMS_summary, User.code ~ Trial, value.var = "Score")
  
  # Calculate delay time difference in minutes
  # Get timestamps for C and CDelay trials
  timestamps <- CMSData[, .(
    C_timestamp = Completed.Timestamp[Trial == "C1"],
    CDelay_timestamp = Completed.Timestamp[Trial == "CDelay"]
  ), by = User.code]
  
  # Calculate time difference in minutes
  timestamps[, Delay_Time_Minutes := as.numeric(difftime(
    as.POSIXct(CDelay_timestamp),
    as.POSIXct(C_timestamp),
    units = "mins"
  ))]
  
  # Merge delay time back into summary
  CMS_summary <- merge(CMS_summary, timestamps[, .(User.code, Delay_Time_Minutes)], by = "User.code")
  
  return(CMS_summary)
}

#' CFT Summary data
#' Generates summary variables for CFT task using data.table
#' @param CFTData data.table containing the long format CFT data
#' @return data.table with summary variables
summaryVarsCFT <- function(CFTData) {
  if (!is.data.table(CFTData)) setDT(CFTData)
  
  # Create summary table with one row per user
  CFT_summary <- CFTData[, .(
    # Calculate test scores
    Test_1 = sum(Trial.result[Block == "Test_1"]=="PASS", na.rm = TRUE),
    Test_3 = sum(Trial.result[Block == "Test_3"]=="PASS", na.rm = TRUE),
    AgeAtTesting = mean(AgeAtTesting, na.rm = TRUE),
    CFT_QC_CFT_ONE_RESPONSE= ifelse(min(as.numeric(Response), na.rm=T) ==  max(as.numeric(Response), na.rm=T), 1,0)
  ), by = User.code]
  
  CFT_summary[, `:=`(
    Raw_Total = Test_1 + Test_3,
    ProRated_Total = (Test_1 + Test_3) *44/23
  ), by = User.code]
  
  # Calculate IQ based on age and total score
  CFT_summary[, IQ := fcase(
    AgeAtTesting >13.75 & ProRated_Total<= 14.5,  57,
    AgeAtTesting >13.75 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  62,
    AgeAtTesting >13.75 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  66,
    AgeAtTesting >13.75 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  70,
    AgeAtTesting >13.75 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  73,
    AgeAtTesting >13.75 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  76,
    AgeAtTesting >13.75 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  79,
    AgeAtTesting >13.75 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  81,
    AgeAtTesting >13.75 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  83,
    AgeAtTesting >13.75 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  84,
    AgeAtTesting >13.75 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  86,
    AgeAtTesting >13.75 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  87,
    AgeAtTesting >13.75 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  89,
    AgeAtTesting >13.75 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  91,
    AgeAtTesting >13.75 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  92,
    AgeAtTesting >13.75 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  94,
    AgeAtTesting >13.75 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  96,
    AgeAtTesting >13.75 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  97,
    AgeAtTesting >13.75 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  99,
    AgeAtTesting >13.75 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  102,
    AgeAtTesting >13.75 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  104.5,
    AgeAtTesting >13.75 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  109,
    AgeAtTesting >13.75 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  113,
    AgeAtTesting >13.75 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  118,
    AgeAtTesting >13.75 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  122,
    AgeAtTesting >13.75 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  127,
    AgeAtTesting >13.75 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  133,
    AgeAtTesting >13.75 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  139,
    AgeAtTesting >13.75 & ProRated_Total > 41.5 & ProRated_Total <=42.5,  145,
    AgeAtTesting >13.75 & ProRated_Total > 42.5 & ProRated_Total <=43.5,  151,
    AgeAtTesting >13.75 & ProRated_Total > 43.5,  157,
    
    AgeAtTesting >13.25 & ProRated_Total<= 14.5,  59, 
    AgeAtTesting >13.25 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  63, 
    AgeAtTesting >13.25 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  68, 
    AgeAtTesting >13.25 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  71, 
    AgeAtTesting >13.25 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  74, 
    AgeAtTesting >13.25 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  77, 
    AgeAtTesting >13.25 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  80, 
    AgeAtTesting >13.25 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  82, 
    AgeAtTesting >13.25 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  84, 
    AgeAtTesting >13.25 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  86, 
    AgeAtTesting >13.25 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  88, 
    AgeAtTesting >13.25 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  89,
    AgeAtTesting >13.25 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  91, 
    AgeAtTesting >13.25 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  93, 
    AgeAtTesting >13.25 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  95, 
    AgeAtTesting >13.25 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  96, 
    AgeAtTesting >13.25 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  98, 
    AgeAtTesting >13.25 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  100, 
    AgeAtTesting >13.25 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  103, 
    AgeAtTesting >13.25 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  106, 
    AgeAtTesting >13.25 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  108.5, 
    AgeAtTesting >13.25 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  112, 
    AgeAtTesting >13.25 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  116, 
    AgeAtTesting >13.25 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  120, 
    AgeAtTesting >13.25 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  124, 
    AgeAtTesting >13.25 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  130, 
    AgeAtTesting >13.25 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  136, 
    AgeAtTesting >13.25 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  142, 
    AgeAtTesting >13.25 & ProRated_Total > 41.5 & ProRated_Total <=42.5,  146, 
    AgeAtTesting >13.25 & ProRated_Total > 42.5 & ProRated_Total <=43.5,  152, 
    AgeAtTesting >13.25 & ProRated_Total > 43.5 ,  160, 
    
    AgeAtTesting >12.75 & ProRated_Total<= 14.5,  61, 
    AgeAtTesting >12.75 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  65, 
    AgeAtTesting >12.75 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  69, 
    AgeAtTesting >12.75 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  72, 
    AgeAtTesting >12.75 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  76, 
    AgeAtTesting >12.75 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  78, 
    AgeAtTesting >12.75 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  81, 
    AgeAtTesting >12.75 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  83, 
    AgeAtTesting >12.75 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  85, 
    AgeAtTesting >12.75 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  87, 
    AgeAtTesting >12.75 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  89, 
    AgeAtTesting >12.75 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  91, 
    AgeAtTesting >12.75 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  93, 
    AgeAtTesting >12.75 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  95, 
    AgeAtTesting >12.75 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  97, 
    AgeAtTesting >12.75 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  99, 
    AgeAtTesting >12.75 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  101, 
    AgeAtTesting >12.75 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  103, 
    AgeAtTesting >12.75 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  105, 
    AgeAtTesting >12.75 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  108, 
    AgeAtTesting >12.75 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  110.5, 
    AgeAtTesting >12.75 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  115, 
    AgeAtTesting >12.75 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  118, 
    AgeAtTesting >12.75 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  122, 
    AgeAtTesting >12.75 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  126, 
    AgeAtTesting >12.75 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  132, 
    AgeAtTesting >12.75 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  138, 
    AgeAtTesting >12.75 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  143, 
    AgeAtTesting >12.75 & ProRated_Total > 41.5 & ProRated_Total <=42.5,  148, 
    AgeAtTesting >12.75 & ProRated_Total > 42.5,  153, 
    
    AgeAtTesting >12.25 & ProRated_Total<= 13.5,  59, 
    AgeAtTesting >12.25 & ProRated_Total > 13.5 & ProRated_Total <=14.5,  64, 
    AgeAtTesting >12.25 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  68, 
    AgeAtTesting >12.25 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  71, 
    AgeAtTesting >12.25 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  74, 
    AgeAtTesting >12.25 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  77, 
    AgeAtTesting >12.25 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  79, 
    AgeAtTesting >12.25 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  82, 
    AgeAtTesting >12.25 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  84, 
    AgeAtTesting >12.25 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  86, 
    AgeAtTesting >12.25 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  89, 
    AgeAtTesting >12.25 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  91, 
    AgeAtTesting >12.25 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  93,
    AgeAtTesting >12.25 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  95,
    AgeAtTesting >12.25 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  98,
    AgeAtTesting >12.25 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  100,
    AgeAtTesting >12.25 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  102,
    AgeAtTesting >12.25 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  104,
    AgeAtTesting >12.25 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  106,
    AgeAtTesting >12.25 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  109,
    AgeAtTesting >12.25 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  111,
    AgeAtTesting >12.25 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  113.5, 
    AgeAtTesting >12.25 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  118, 
    AgeAtTesting >12.25 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  122, 
    AgeAtTesting >12.25 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  125, 
    AgeAtTesting >12.25 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  129, 
    AgeAtTesting >12.25 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  134, 
    AgeAtTesting >12.25 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  140, 
    AgeAtTesting >12.25 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  144, 
    AgeAtTesting >12.25 & ProRated_Total > 41.5 & ProRated_Total <=42.5,  149, 
    AgeAtTesting >12.25 & ProRated_Total > 42.5,  155, 
    
    AgeAtTesting >11.75 & ProRated_Total<= 12.5,  59, 
    AgeAtTesting >11.75 & ProRated_Total > 12.5 & ProRated_Total <=13.5,  64, 
    AgeAtTesting >11.75 & ProRated_Total > 13.5 & ProRated_Total <=14.5,  68, 
    AgeAtTesting >11.75 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  71, 
    AgeAtTesting >11.75 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  74, 
    AgeAtTesting >11.75 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  76,
    AgeAtTesting >11.75 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  79,
    AgeAtTesting >11.75 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  81,
    AgeAtTesting >11.75 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  84,
    AgeAtTesting >11.75 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  86,
    AgeAtTesting >11.75 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  88,
    AgeAtTesting >11.75 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  91,
    AgeAtTesting >11.75 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  93,
    AgeAtTesting >11.75 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  95,
    AgeAtTesting >11.75 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  97,
    AgeAtTesting >11.75 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  100,
    AgeAtTesting >11.75 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  102,
    AgeAtTesting >11.75 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  104,
    AgeAtTesting >11.75 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  107,
    AgeAtTesting >11.75 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  109,
    AgeAtTesting >11.75 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  112,
    AgeAtTesting >11.75 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  115,
    AgeAtTesting >11.75 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  117.5,
    AgeAtTesting >11.75 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  121,
    AgeAtTesting >11.75 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  125,
    AgeAtTesting >11.75 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  128,
    AgeAtTesting >11.75 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  132,
    AgeAtTesting >11.75 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  135,
    AgeAtTesting >11.75 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  142,
    AgeAtTesting >11.75 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  145,
    AgeAtTesting >11.75 & ProRated_Total > 41.5 ,  151,
    
    AgeAtTesting >11.25 & ProRated_Total<= 12.5,  62,
    AgeAtTesting >11.25 & ProRated_Total > 12.5 & ProRated_Total <=13.5,  66,
    AgeAtTesting >11.25 & ProRated_Total > 13.5 & ProRated_Total <=14.5,  69,
    AgeAtTesting >11.25 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  72,
    AgeAtTesting >11.25 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  75,
    AgeAtTesting >11.25 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  78,
    AgeAtTesting >11.25 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  80,
    AgeAtTesting >11.25 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  82,
    AgeAtTesting >11.25 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  85,
    AgeAtTesting >11.25 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  87,
    AgeAtTesting >11.25 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  90,
    AgeAtTesting >11.25 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  93,
    AgeAtTesting >11.25 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  95,
    AgeAtTesting >11.25 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  97,
    AgeAtTesting >11.25 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  100,
    AgeAtTesting >11.25 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  103,
    AgeAtTesting >11.25 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  105,
    AgeAtTesting >11.25 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  108,
    AgeAtTesting >11.25 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  110,
    AgeAtTesting >11.25 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  113,
    AgeAtTesting >11.25 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  116,
    AgeAtTesting >11.25 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  119,
    AgeAtTesting >11.25 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  121.5,
    AgeAtTesting >11.25 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  125,
    AgeAtTesting >11.25 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  128,
    AgeAtTesting >11.25 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  131,
    AgeAtTesting >11.25 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  135,
    AgeAtTesting >11.25 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  138,
    AgeAtTesting >11.25 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  143,
    AgeAtTesting >11.25 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  147,
    AgeAtTesting >11.25 & ProRated_Total > 41.5 ,  152,
    
    AgeAtTesting >10.75 & ProRated_Total<= 11.5,  60,
    AgeAtTesting >10.75 & ProRated_Total > 11.5 & ProRated_Total <=12.5,  64,
    AgeAtTesting >10.75 & ProRated_Total > 12.5 & ProRated_Total <=13.5,  68,
    AgeAtTesting >10.75 & ProRated_Total > 13.5 & ProRated_Total <=14.5,  71,
    AgeAtTesting >10.75 & ProRated_Total > 14.5 & ProRated_Total <=15.5,  73,
    AgeAtTesting >10.75 & ProRated_Total > 15.5 & ProRated_Total <=16.5,  76,
    AgeAtTesting >10.75 & ProRated_Total > 16.5 & ProRated_Total <=17.5,  79,
    AgeAtTesting >10.75 & ProRated_Total > 17.5 & ProRated_Total <=18.5,  81,
    AgeAtTesting >10.75 & ProRated_Total > 18.5 & ProRated_Total <=19.5,  84,
    AgeAtTesting >10.75 & ProRated_Total > 19.5 & ProRated_Total <=20.5,  87,
    AgeAtTesting >10.75 & ProRated_Total > 20.5 & ProRated_Total <=21.5,  89,
    AgeAtTesting >10.75 & ProRated_Total > 21.5 & ProRated_Total <=22.5,  92,
    AgeAtTesting >10.75 & ProRated_Total > 22.5 & ProRated_Total <=23.5,  95,
    AgeAtTesting >10.75 & ProRated_Total > 23.5 & ProRated_Total <=24.5,  97,
    AgeAtTesting >10.75 & ProRated_Total > 24.5 & ProRated_Total <=25.5,  100,
    AgeAtTesting >10.75 & ProRated_Total > 25.5 & ProRated_Total <=26.5,  103,
    AgeAtTesting >10.75 & ProRated_Total > 26.5 & ProRated_Total <=27.5,  105,
    AgeAtTesting >10.75 & ProRated_Total > 27.5 & ProRated_Total <=28.5,  108,
    AgeAtTesting >10.75 & ProRated_Total > 28.5 & ProRated_Total <=29.5,  111,
    AgeAtTesting >10.75 & ProRated_Total > 29.5 & ProRated_Total <=30.5,  113,
    AgeAtTesting >10.75 & ProRated_Total > 30.5 & ProRated_Total <=31.5,  116,
    AgeAtTesting >10.75 & ProRated_Total > 31.5 & ProRated_Total <=32.5,  119,
    AgeAtTesting >10.75 & ProRated_Total > 32.5 & ProRated_Total <=33.5,  121,
    AgeAtTesting >10.75 & ProRated_Total > 33.5 & ProRated_Total <=34.5,  123.5,
    AgeAtTesting >10.75 & ProRated_Total > 34.5 & ProRated_Total <=35.5,  127,
    AgeAtTesting >10.75 & ProRated_Total > 35.5 & ProRated_Total <=36.5,  130,
    AgeAtTesting >10.75 & ProRated_Total > 36.5 & ProRated_Total <=37.5,  133,
    AgeAtTesting >10.75 & ProRated_Total > 37.5 & ProRated_Total <=38.5,  136,
    AgeAtTesting >10.75 & ProRated_Total > 38.5 & ProRated_Total <=39.5,  139,
    AgeAtTesting >10.75 & ProRated_Total > 39.5 & ProRated_Total <=40.5,  144,
    AgeAtTesting >10.75 & ProRated_Total > 40.5 & ProRated_Total <=41.5,  148,
    AgeAtTesting >10.75 & ProRated_Total > 41.5 ,  153
  )]
  
  return(CFT_summary)
}

#' SWMSummary data
#' Generates the summary variables for SWM task
#' NB This could be extended to provide a Strategy score like the cantab version if desired
#' @param SWMData dataframe containing the long format SWM data
#' @keywords download dataset
summaryVarsSWM <- function(SWMData) {
  # Convert to data.table if not already
  setDT(SWMData)
  
  # Drop Intro data
  SWMData<-SWMData[!is.na(Block) & Block != "Intro", ]
  
  # Function to calculate longest gap between searches
  f2 <- function(x) {
    runs <- rle(x == 0L)
    # If there are no gaps (no runs of zeros), return 0 as integer
    if (!any(runs$values)) return(0L)
    # Otherwise return the max length of zero runs as integer
    as.integer(max(runs$lengths[runs$values]))
  }
  
  # Calculate longest gap between searches for each block
  lgtsp <- SWMData[, .(User.code, Block_Trial = Block, Between.search.errors)]
  lgtsp[, c("Block", "Trial") := tstrsplit(Block_Trial, "_", fixed = TRUE)]
  
  # Calculate longest gap for each block (averaging across trials)
  lgtsp <- lgtsp[, .(lgtsp = f2(Between.search.errors)), by = .(User.code, Block, Trial)]
  lgtsp <- lgtsp[, .(lgtsp = mean(lgtsp)), by = .(User.code, Block)]
  
  # Reshape to wide format
  lgtsp <- dcast(lgtsp, User.code ~ Block, value.var = "lgtsp")
  setnames(lgtsp, c("4", "6", "8", "10"), c("t4_span", "t6_span", "t8_span", "t10_span"))
  
  # Calculate total errors by user and block
  errors <- SWMData[, .(BSE = sum(Between.search.errors, na.rm = TRUE),
                        WSE = sum(Within.search.errors, na.rm = TRUE)),
                    by = .(User.code, Block)]
  
  # Reshape errors to wide format
  errors <- dcast(melt(errors, id.vars = c("User.code", "Block")), 
                  User.code ~ variable + Block)
  
  # Calculate total errors
  errors[, BSE_TOTAL_8 := BSE_4_1 + BSE_6_1 + BSE_8_1 + BSE_4_2 + BSE_6_2 + BSE_8_2 ]
  errors[, WSE_TOTAL_8 := WSE_4_1 + WSE_6_1 + WSE_8_1 + WSE_4_2 + WSE_6_2 + WSE_8_2 ]
  errors[, BSE_TOTAL_10 := BSE_TOTAL_8 + BSE_10_1 + BSE_10_2 ]
  errors[, WSE_TOTAL_10 := WSE_TOTAL_8 + WSE_10_1 + WSE_10_2 ]
  
  # Merge with lgtsp data
  result <- merge(errors, lgtsp, by = "User.code")
  
  return(result)
}

#' Safely convert columns to numeric, handling DK/REFUSE and non-numeric values
#' @param .SD Subset of data containing all rows for the selected columns
#' @param cols Character vector of column names to convert
#' @param fun Function to apply to the converted columns (default: rowSums)
#' @param ... Additional arguments passed to fun
#' @return A numeric vector of the calculated values
#' @export
safeSD <- function(.SD, cols, fun = rowSums, ...) {
  # Suppress warnings about NA introduction
  suppressWarnings({
    # Convert each column to numeric, treating DK/REFUSE as NA
    numeric_cols <- as.matrix(
      .SD[, lapply(.SD, function(x) {
        # Convert to character first to handle factor columns
        x <- as.character(x)
        # Replace DK/REFUSE with NA
        x[x %in% c("DK", "REFUSE")] <- NA
        # Convert to numeric
        as.numeric(x)
      }), .SDcols = cols]
    )
    
    # Apply the requested function (default: rowSums)
    fun(numeric_cols, ...)
  })
}


#' Recode variables according to a function, creating new columns with 'R' suffix
#' @param dt Data table
#' @param varlist List of variables to recode
#' @param fun Function to apply for recoding
#' @return Data table with new recoded variables
#' @export
recodeVariables <- function(dt, varlist, fun) {
  if (!is.data.table(dt)) setDT(dt)
  
  for (pattern in varlist) {
    # Find matching columns
    matching_cols <- grep(pattern, names(dt), value = TRUE)
    if (length(matching_cols) > 1) {
      stop(paste('Reverse token', pattern, 'does not uniquely identify one variable'))
    }
    if (length(matching_cols) == 0) {
      warning(paste('No matching column found for pattern:', pattern))
      next
    }
    
    # Create new column name with R suffix
    new_col <- paste0(matching_cols, "R")
    
    # Create the new column with recoded values, suppressing NA warnings
    suppressWarnings({
      dt[, (new_col) := {
        x <- get(matching_cols)
        # Convert to character to handle factors
        x <- as.character(x)
        # Only recode non-missing values
        x_num <- as.numeric(x)
        x_num[!is.na(x_num)] <- fun(x_num[!is.na(x_num)])
        x_num
      }]
    })
  }
  
  return(dt)
}



#' Derive questionnaire variables for CUSP
#' @param Qs data.table containing questionnaire data
#' @return data.table with derived variables added
#' @export
deriveQuestionnaireVariables <- function(Qs) {
  if (!is.data.table(Qs)) setDT(Qs)
  
  # First reverse code the SURPS and SES items (5-x formula)
  reverseVariables <- c(
    # SURPS items
    'A1_a$', 'A1_d$', 'A1_g$', 'A1_m$', 'A1_t$', 'A1_w$',
    # SES RC items
    'J13_02$', 'J13_05$', 'J13_06$', 'J13_08$', 'J13_09$'
  )
  Qs <- recodeVariables(Qs, reverseVariables, function(x) {5-x})
  
  # Reverse code SDQ items (2-x formula)
  sdqReverseVariables <- c('J12_07$', 'J12_11$', 'J12_14$', 'J12_21$', 'J12_25$')
  Qs <- recodeVariables(Qs, sdqReverseVariables, function(x) {2-x})
  
  # Recode C2_D for CUDIT scoring
  Qs[, C2_D_CUDIT := fcase(
    C2_D %in% c("0", "1"), "0",
    C2_D == "2", "1",
    C2_D == "3", "2",
    C2_D == "4", "3",
    C2_D %in% c("5", "6"), "4",
    default = NA_character_
  )]
  Qs[, C2_D_CUDIT := as.numeric(C2_D_CUDIT)]
  
  # Recode G4_05 for CUDIT scoring
  Qs[, G4_05_CUDIT := fcase(
    G4_05 == "0", "0",
    G4_05 == "1", "2",
    G4_05 %in% c("2", "3", "4"), "4",
    default = NA_character_
  )]
  Qs[, G4_05_CUDIT := as.numeric(G4_05_CUDIT)]
  
  # SURPS variables - note use of 'R' suffix for reversed items
  surps_vars <- list(
    AS = c("A1_h", "A1_j", "A1_n", "A1_r", "A1_u"),
    NT = c("A1_aR", "A1_dR", "A1_gR", "A1_mR", "A1_q", "A1_tR", "A1_wR"),
    IMP = c("A1_b", "A1_e", "A1_k", "A1_o", "A1_v"),
    SS = c("A1_c", "A1_f", "A1_i", "A1_l", "A1_p", "A1_s")
  )
  
  # Calculate SURPS subscales
  for (scale in names(surps_vars)) {
    Qs[, paste0("SURPS_", scale) := safeSD(.SD, surps_vars[[scale]])]
  }
  
  # Calculate SURPS total as sum of subscales
  Qs[, SURPS_Total := SURPS_AS + SURPS_NT + SURPS_IMP + SURPS_SS]
  
  # CRAFFT variables
  crafft_part_a <- c("C1A_A", "C1A_B", "C1A_C")
  crafft_part_b <- c("C1B_A", "C1B_B", "C1B_C", "C1B_D", "C1B_E", "C1B_F")
  
  # Calculate CRAFFT scores
  Qs[, CRAFFT_PartA := safeSD(.SD, crafft_part_a)]
  Qs[, CRAFFT_PartB := safeSD(.SD, crafft_part_b)]
  Qs[, CRAFFT_Total := CRAFFT_PartA + CRAFFT_PartB]
  
  # SUMM variables (C5_A_01 to C5_A_06)
  summ_vars <- paste0("C5_A_0", 1:6)
  Qs[, SUMM_Total := safeSD(.SD, summ_vars)]
  
  # MAAQ variables (C6_A_01 to C6_A_08)
  maaq_vars <- paste0("C6_A_0", 1:8)
  Qs[, MAAQ_Total := safeSD(.SD, maaq_vars)]
  
  # AUDIT variables
  audit_vars <- c("C2_A", "D2_01", "D2_02", "D2_03", "D2_04", "D2_05", "D2_06", "D1", "D3", "D4")
  Qs[, AUDIT_Total := safeSD(.SD, audit_vars)]
  
  # CAST variables
  cast_vars <- paste0("G4_0", 1:6)
  Qs[, CAST_Total := safeSD(.SD, cast_vars)]
  
  # CUDIT variables - note use of recoded variables
  cudit_vars <- c("C2_D_CUDIT", "G4_07", "G4_08", "G4_09", "G4_10", "G4_11", "G4_05_CUDIT")
  Qs[, CUDIT_Total := safeSD(.SD, cudit_vars), .SDcols = cudit_vars]
  Qs[, CUDIT_Mean := safeSD(.SD, cudit_vars, fun = rowMeans, na.rm = TRUE), .SDcols = cudit_vars]
  Qs[, CUDIT_Median := safeSD(.SD, cudit_vars, fun = function(x) apply(x, 1, median, na.rm = TRUE)), .SDcols = cudit_vars]
  
  # BSI variables
  bsi_dep_vars <- paste0("J8_0", 1:6)
  bsi_anx_vars <- paste0("J8_", sprintf("%02d", 7:12))
  
  Qs[, BSI_Dep := safeSD(.SD, bsi_dep_vars), .SDcols = bsi_dep_vars]
  Qs[, BSI_Anx := safeSD(.SD, bsi_anx_vars), .SDcols = bsi_anx_vars]
  Qs[, BSI_Total := BSI_Dep + BSI_Anx]
  
  # Brief COPE variables
  brief_cope_vars <- list(
    SD = c("J9_01", "J9_19"),
    AC = c("J9_02", "J9_07"),
    D = c("J9_03", "J9_08"),
    SU = c("J9_04", "J9_11"),
    UES = c("J9_05", "J9_15"),
    UIS = c("J9_10", "J9_23"),
    BD = c("J9_06", "J9_16"),
    V = c("J9_09", "J9_21"),
    PR = c("J9_12", "J9_17"),
    P = c("J9_14", "J9_25"),
    H = c("J9_18", "J9_28"),
    A = c("J9_20", "J9_24"),
    R = c("J9_22", "J9_27"),
    SB = c("J9_13", "J9_26")
  )
  
  # Calculate Brief COPE subscales
  for (scale in names(brief_cope_vars)) {
    Qs[, paste0("Brief_COPE_", scale) := safeSD(.SD, brief_cope_vars[[scale]]), 
       .SDcols = brief_cope_vars[[scale]]]
  }
  
  # Calculate Brief COPE total as sum of all subscales
  brief_cope_cols <- paste0("Brief_COPE_", names(brief_cope_vars))
  Qs[, Brief_COPE_Total := rowSums(.SD), .SDcols = brief_cope_cols]
  
  # APSS variables
  apss_vars <- c(paste0("J10_0", 1:5), "J10_08", "J10_09")
  Qs[, APSS_Total := safeSD(.SD, apss_vars)]
  
  # ATQ variables (J11_01 to J11_20)
  atq_vars <- paste0("J11_", sprintf("%02d", 1:20))
  Qs[, ATQ_Total := safeSD(.SD, atq_vars)]
  
  # SES variables - note use of 'R' suffix for reversed items
  ses_ri <- c("J13_01", "J13_03", "J13_04", "J13_07", "J13_10")
  ses_rc <- c("J13_02R", "J13_05R", "J13_06R", "J13_08R", "J13_09R")
  
  Qs[, SES_RI := safeSD(.SD, ses_ri), .SDcols = ses_ri]
  Qs[, SES_RC := safeSD(.SD, ses_rc), .SDcols = ses_rc]
  Qs[, SES_Total := SES_RI + SES_RC]
  
  # SDQ variables - note use of 'R' suffix for reversed items
  sdq_vars <- list(
    ES = c("J12_03", "J12_08", "J12_13", "J12_16", "J12_24"),
    CS = c("J12_05", "J12_07R", "J12_12", "J12_18", "J12_22"),
    H = c("J12_02", "J12_10", "J12_15", "J12_21R", "J12_25R"),
    PP = c("J12_06", "J12_11R", "J12_14R", "J12_19", "J12_23"),
    PS = c("J12_01", "J12_04", "J12_09", "J12_17", "J12_20")
  )
  
  # Calculate SDQ subscales
  for (scale in names(sdq_vars)) {
    Qs[, paste0("SDQ_", scale) := safeSD(.SD, sdq_vars[[scale]]), 
       .SDcols = sdq_vars[[scale]]]
  }
  
  # Calculate SDQ total as sum of all subscales
  sdq_cols <- paste0("SDQ_", names(sdq_vars))
  Qs[, SDQ_Total := rowSums(.SD), .SDcols = sdq_cols]
  
  return(Qs)
} 

