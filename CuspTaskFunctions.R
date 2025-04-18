#' Task Summary Functions
#' Optimized versions using data.table for better performance
#' @import data.table


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
  
  # Calculate scores
  PALPData[, `:=`(
    Om = fifelse(!grepl("space", Response) & grepl("^I", Trial.result), 1L, 0L),
    Com = fifelse(grepl("space", Response) & grepl("^I", Trial.result), 1L, 0L),
    condition = fifelse(grepl("RR", Block), "RR", "RP")
  )]
  
  # Calculate QC flags
  PALPData[, `:=`(
    PALP_QC_PALP_WORSE_THAN_CHANCE = fifelse(
      mean(Om, na.rm = TRUE) < 0.5 & mean(Com, na.rm = TRUE) < 0.5,
      1L, 0L
    )
  ), by = User.code]
  
  # Calculate summary statistics
  PALPData[, `:=`(
    Hits = sum(Om, na.rm = TRUE),
    CorrectNR = sum(Com, na.rm = TRUE),
    meanRT = mean(Response.time..ms., na.rm = TRUE)
  ), by = .(User.code, condition)]
  
  # Calculate aprime and bppd
  aprime <- function(hit, fa) {
    (0.5 + (hit - fa) * (1 + hit - fa) / (4 * hit * (1 - fa)))
  }
  
  bppd <- function(hit, fa) {
    ((1-hit)*(1-fa)-hit*fa) / ((1-hit)*(1-fa)+hit*fa)
  }
  
  PALPData[, `:=`(
    aprime = aprime(Hits, CorrectNR),
    bppd = bppd(Hits, CorrectNR)
  ), by = .(User.code, condition)]
  
  # Reshape to wide format
  PALPData <- dcast(
    melt(PALPData, id.vars = c("User.code", "condition")),
    User.code ~ condition + variable,
    fun.aggregate = mean,
    na.rm = TRUE
  )
  
  return(PALPData)
}

#' CMS Summary data
#' Generates summary variables for CMS task using data.table
#' @param CMSData data.table containing the long format CMS data
#' @return data.table with summary variables
summaryVarsCMS <- function(CMSData) {
  if (!is.data.table(CMSData)) setDT(CMSData)
  
  # Calculate scores
  CMSData[, `:=`(
    Correct = fifelse(Trial.result == "correct", 1L, 0L),
    Incorrect = fifelse(Trial.result == "incorrect", 1L, 0L),
    Omission = fifelse(Trial.result == "omission", 1L, 0L)
  )]
  
  # Calculate summary statistics
  CMSData[, `:=`(
    TotalCorrect = sum(Correct, na.rm = TRUE),
    TotalIncorrect = sum(Incorrect, na.rm = TRUE),
    TotalOmissions = sum(Omission, na.rm = TRUE),
    MeanRT = mean(Response.time..ms., na.rm = TRUE),
    SDRT = sd(Response.time..ms., na.rm = TRUE)
  ), by = User.code]
  
  # Calculate accuracy
  CMSData[, Accuracy := TotalCorrect / (TotalCorrect + TotalIncorrect + TotalOmissions)]
  
  return(CMSData)
}

#' CFT Summary data
#' Generates summary variables for CFT task using data.table
#' @param CFTData data.table containing the long format CFT data
#' @return data.table with summary variables
summaryVarsCFT <- function(CFTData) {
  if (!is.data.table(CFTData)) setDT(CFTData)
  
  # Calculate scores
  CFTData[, `:=`(
    Test_1 = sum(Trial.result == "correct", na.rm = TRUE),
    Test_3 = sum(Trial.result == "correct" & Block == "Test_3", na.rm = TRUE),
    Raw_Total = Test_1 + Test_3,
    ProRated_Total = Raw_Total * (60 / nrow(.SD))
  ), by = User.code]
  
  # Calculate QC flags
  CFTData[, CFT_QC_CFT_ONE_RESPONSE := fifelse(
    all(Trial.result == "correct") | all(Trial.result == "incorrect"),
    1L, 0L
  ), by = User.code]
  
  # Calculate IQ based on age and total score
  CFTData[, IQ := fcase(
    # Age > 13.25
    AgeAtTesting > 13.25 & ProRated_Total > 43.5, 160,
    AgeAtTesting > 13.25 & ProRated_Total > 42.5 & ProRated_Total <= 43.5, 152,
    AgeAtTesting > 13.25 & ProRated_Total > 41.5 & ProRated_Total <= 42.5, 146,
    AgeAtTesting > 13.25 & ProRated_Total > 40.5 & ProRated_Total <= 41.5, 142,
    AgeAtTesting > 13.25 & ProRated_Total > 39.5 & ProRated_Total <= 40.5, 136,
    AgeAtTesting > 13.25 & ProRated_Total > 38.5 & ProRated_Total <= 39.5, 130,
    AgeAtTesting > 13.25 & ProRated_Total > 37.5 & ProRated_Total <= 38.5, 124,
    AgeAtTesting > 13.25 & ProRated_Total > 36.5 & ProRated_Total <= 37.5, 120,
    AgeAtTesting > 13.25 & ProRated_Total > 35.5 & ProRated_Total <= 36.5, 116,
    AgeAtTesting > 13.25 & ProRated_Total > 34.5 & ProRated_Total <= 35.5, 112,
    AgeAtTesting > 13.25 & ProRated_Total > 33.5 & ProRated_Total <= 34.5, 108.5,
    AgeAtTesting > 13.25 & ProRated_Total > 32.5 & ProRated_Total <= 33.5, 106,
    AgeAtTesting > 13.25 & ProRated_Total > 31.5 & ProRated_Total <= 32.5, 103,
    AgeAtTesting > 13.25 & ProRated_Total > 30.5 & ProRated_Total <= 31.5, 100,
    AgeAtTesting > 13.25 & ProRated_Total > 29.5 & ProRated_Total <= 30.5, 98,
    AgeAtTesting > 13.25 & ProRated_Total > 28.5 & ProRated_Total <= 29.5, 96,
    AgeAtTesting > 13.25 & ProRated_Total > 27.5 & ProRated_Total <= 28.5, 95,
    AgeAtTesting > 13.25 & ProRated_Total > 26.5 & ProRated_Total <= 27.5, 93,
    AgeAtTesting > 13.25 & ProRated_Total > 25.5 & ProRated_Total <= 26.5, 91,
    AgeAtTesting > 13.25 & ProRated_Total > 24.5 & ProRated_Total <= 25.5, 89,
    AgeAtTesting > 13.25 & ProRated_Total > 23.5 & ProRated_Total <= 24.5, 88,
    AgeAtTesting > 13.25 & ProRated_Total > 22.5 & ProRated_Total <= 23.5, 86,
    AgeAtTesting > 13.25 & ProRated_Total > 21.5 & ProRated_Total <= 22.5, 84,
    
    # Age > 12.75
    AgeAtTesting > 12.75 & ProRated_Total > 42.5, 153,
    AgeAtTesting > 12.75 & ProRated_Total > 41.5 & ProRated_Total <= 42.5, 148,
    AgeAtTesting > 12.75 & ProRated_Total > 40.5 & ProRated_Total <= 41.5, 143,
    AgeAtTesting > 12.75 & ProRated_Total > 39.5 & ProRated_Total <= 40.5, 138,
    AgeAtTesting > 12.75 & ProRated_Total > 38.5 & ProRated_Total <= 39.5, 132,
    AgeAtTesting > 12.75 & ProRated_Total > 37.5 & ProRated_Total <= 38.5, 126,
    AgeAtTesting > 12.75 & ProRated_Total > 36.5 & ProRated_Total <= 37.5, 122,
    AgeAtTesting > 12.75 & ProRated_Total > 35.5 & ProRated_Total <= 36.5, 118,
    AgeAtTesting > 12.75 & ProRated_Total > 34.5 & ProRated_Total <= 35.5, 115,
    AgeAtTesting > 12.75 & ProRated_Total > 33.5 & ProRated_Total <= 34.5, 110.5,
    AgeAtTesting > 12.75 & ProRated_Total > 32.5 & ProRated_Total <= 33.5, 108,
    AgeAtTesting > 12.75 & ProRated_Total > 31.5 & ProRated_Total <= 32.5, 105,
    AgeAtTesting > 12.75 & ProRated_Total > 30.5 & ProRated_Total <= 31.5, 103,
    AgeAtTesting > 12.75 & ProRated_Total > 29.5 & ProRated_Total <= 30.5, 101,
    AgeAtTesting > 12.75 & ProRated_Total > 28.5 & ProRated_Total <= 29.5, 99,
    AgeAtTesting > 12.75 & ProRated_Total > 27.5 & ProRated_Total <= 28.5, 97,
    AgeAtTesting > 12.75 & ProRated_Total > 26.5 & ProRated_Total <= 27.5, 95,
    AgeAtTesting > 12.75 & ProRated_Total > 25.5 & ProRated_Total <= 26.5, 93,
    AgeAtTesting > 12.75 & ProRated_Total > 24.5 & ProRated_Total <= 25.5, 91,
    AgeAtTesting > 12.75 & ProRated_Total > 23.5 & ProRated_Total <= 24.5, 89,
    AgeAtTesting > 12.75 & ProRated_Total > 22.5 & ProRated_Total <= 23.5, 87,
    AgeAtTesting > 12.75 & ProRated_Total > 21.5 & ProRated_Total <= 22.5, 85,
    AgeAtTesting > 12.75 & ProRated_Total > 20.5 & ProRated_Total <= 21.5, 83,
    AgeAtTesting > 12.75 & ProRated_Total > 19.5 & ProRated_Total <= 20.5, 81,
    AgeAtTesting > 12.75 & ProRated_Total > 18.5 & ProRated_Total <= 19.5, 78,
    AgeAtTesting > 12.75 & ProRated_Total > 17.5 & ProRated_Total <= 18.5, 76,
    AgeAtTesting > 12.75 & ProRated_Total > 16.5 & ProRated_Total <= 17.5, 72,
    AgeAtTesting > 12.75 & ProRated_Total > 15.5 & ProRated_Total <= 16.5, 69,
    AgeAtTesting > 12.75 & ProRated_Total > 14.5 & ProRated_Total <= 15.5, 65,
    AgeAtTesting > 12.75 & ProRated_Total <= 14.5, 61,
    
    # Age > 12.25
    AgeAtTesting > 12.25 & ProRated_Total > 42.5, 155,
    AgeAtTesting > 12.25 & ProRated_Total > 41.5 & ProRated_Total <= 42.5, 149,
    AgeAtTesting > 12.25 & ProRated_Total > 40.5 & ProRated_Total <= 41.5, 144,
    AgeAtTesting > 12.25 & ProRated_Total > 39.5 & ProRated_Total <= 40.5, 140,
    AgeAtTesting > 12.25 & ProRated_Total > 38.5 & ProRated_Total <= 39.5, 134,
    AgeAtTesting > 12.25 & ProRated_Total > 37.5 & ProRated_Total <= 38.5, 129,
    AgeAtTesting > 12.25 & ProRated_Total > 36.5 & ProRated_Total <= 37.5, 125,
    AgeAtTesting > 12.25 & ProRated_Total > 35.5 & ProRated_Total <= 36.5, 122,
    AgeAtTesting > 12.25 & ProRated_Total > 34.5 & ProRated_Total <= 35.5, 118,
    AgeAtTesting > 12.25 & ProRated_Total > 33.5 & ProRated_Total <= 34.5, 113.5,
    AgeAtTesting > 12.25 & ProRated_Total > 32.5 & ProRated_Total <= 33.5, 111,
    AgeAtTesting > 12.25 & ProRated_Total > 31.5 & ProRated_Total <= 32.5, 109,
    AgeAtTesting > 12.25 & ProRated_Total > 30.5 & ProRated_Total <= 31.5, 106,
    AgeAtTesting > 12.25 & ProRated_Total > 29.5 & ProRated_Total <= 30.5, 104,
    AgeAtTesting > 12.25 & ProRated_Total > 28.5 & ProRated_Total <= 29.5, 102,
    AgeAtTesting > 12.25 & ProRated_Total > 27.5 & ProRated_Total <= 28.5, 100,
    AgeAtTesting > 12.25 & ProRated_Total > 26.5 & ProRated_Total <= 27.5, 98,
    AgeAtTesting > 12.25 & ProRated_Total > 25.5 & ProRated_Total <= 26.5, 95,
    AgeAtTesting > 12.25 & ProRated_Total > 24.5 & ProRated_Total <= 25.5, 93,
    AgeAtTesting > 12.25 & ProRated_Total > 23.5 & ProRated_Total <= 24.5, 91,
    AgeAtTesting > 12.25 & ProRated_Total > 22.5 & ProRated_Total <= 23.5, 89,
    AgeAtTesting > 12.25 & ProRated_Total > 21.5 & ProRated_Total <= 22.5, 86,
    AgeAtTesting > 12.25 & ProRated_Total > 20.5 & ProRated_Total <= 21.5, 84,
    AgeAtTesting > 12.25 & ProRated_Total > 19.5 & ProRated_Total <= 20.5, 82,
    AgeAtTesting > 12.25 & ProRated_Total > 18.5 & ProRated_Total <= 19.5, 79,
    AgeAtTesting > 12.25 & ProRated_Total > 17.5 & ProRated_Total <= 18.5, 77,
    AgeAtTesting > 12.25 & ProRated_Total > 16.5 & ProRated_Total <= 17.5, 74,
    AgeAtTesting > 12.25 & ProRated_Total > 15.5 & ProRated_Total <= 16.5, 71,
    AgeAtTesting > 12.25 & ProRated_Total > 14.5 & ProRated_Total <= 15.5, 68,
    AgeAtTesting > 12.25 & ProRated_Total > 13.5 & ProRated_Total <= 14.5, 64,
    AgeAtTesting > 12.25 & ProRated_Total <= 13.5, 59,
    
    # Age > 11.75
    AgeAtTesting > 11.75 & ProRated_Total > 42.5, 155,
    AgeAtTesting > 11.75 & ProRated_Total > 41.5 & ProRated_Total <= 42.5, 149,
    AgeAtTesting > 11.75 & ProRated_Total > 40.5 & ProRated_Total <= 41.5, 144,
    AgeAtTesting > 11.75 & ProRated_Total > 39.5 & ProRated_Total <= 40.5, 140,
    AgeAtTesting > 11.75 & ProRated_Total > 38.5 & ProRated_Total <= 39.5, 134,
    AgeAtTesting > 11.75 & ProRated_Total > 37.5 & ProRated_Total <= 38.5, 129,
    AgeAtTesting > 11.75 & ProRated_Total > 36.5 & ProRated_Total <= 37.5, 125,
    AgeAtTesting > 11.75 & ProRated_Total > 35.5 & ProRated_Total <= 36.5, 122,
    AgeAtTesting > 11.75 & ProRated_Total > 34.5 & ProRated_Total <= 35.5, 118,
    AgeAtTesting > 11.75 & ProRated_Total > 33.5 & ProRated_Total <= 34.5, 113.5,
    AgeAtTesting > 11.75 & ProRated_Total > 32.5 & ProRated_Total <= 33.5, 111,
    AgeAtTesting > 11.75 & ProRated_Total > 31.5 & ProRated_Total <= 32.5, 109,
    AgeAtTesting > 11.75 & ProRated_Total > 30.5 & ProRated_Total <= 31.5, 106,
    AgeAtTesting > 11.75 & ProRated_Total > 29.5 & ProRated_Total <= 30.5, 104,
    AgeAtTesting > 11.75 & ProRated_Total > 28.5 & ProRated_Total <= 29.5, 102,
    AgeAtTesting > 11.75 & ProRated_Total > 27.5 & ProRated_Total <= 28.5, 100,
    AgeAtTesting > 11.75 & ProRated_Total > 26.5 & ProRated_Total <= 27.5, 98,
    AgeAtTesting > 11.75 & ProRated_Total > 25.5 & ProRated_Total <= 26.5, 95,
    AgeAtTesting > 11.75 & ProRated_Total > 24.5 & ProRated_Total <= 25.5, 93,
    AgeAtTesting > 11.75 & ProRated_Total > 23.5 & ProRated_Total <= 24.5, 91,
    AgeAtTesting > 11.75 & ProRated_Total > 22.5 & ProRated_Total <= 23.5, 88,
    AgeAtTesting > 11.75 & ProRated_Total > 21.5 & ProRated_Total <= 22.5, 86,
    AgeAtTesting > 11.75 & ProRated_Total > 20.5 & ProRated_Total <= 21.5, 84,
    AgeAtTesting > 11.75 & ProRated_Total > 19.5 & ProRated_Total <= 20.5, 81,
    AgeAtTesting > 11.75 & ProRated_Total > 18.5 & ProRated_Total <= 19.5, 79,
    AgeAtTesting > 11.75 & ProRated_Total > 17.5 & ProRated_Total <= 18.5, 76,
    AgeAtTesting > 11.75 & ProRated_Total > 16.5 & ProRated_Total <= 17.5, 74,
    AgeAtTesting > 11.75 & ProRated_Total > 15.5 & ProRated_Total <= 16.5, 71,
    AgeAtTesting > 11.75 & ProRated_Total > 14.5 & ProRated_Total <= 15.5, 68,
    AgeAtTesting > 11.75 & ProRated_Total > 13.5 & ProRated_Total <= 14.5, 64,
    AgeAtTesting > 11.75 & ProRated_Total > 12.5 & ProRated_Total <= 13.5, 59,
    AgeAtTesting > 11.75 & ProRated_Total <= 12.5, 59,
    
    default = NA_real_
  )]
  
  return(CFTData)
}

#' SWM Summary data
#' Generates summary variables for SWM task using data.table
#' @param SWMData data.table containing the long format SWM data
#' @return data.table with summary variables
summaryVarsSWM <- function(SWMData) {
  if (!is.data.table(SWMData)) setDT(SWMData)
  
  # Calculate scores
  SWMData[, `:=`(
    Correct = fifelse(Trial.result == "correct", 1L, 0L),
    Incorrect = fifelse(Trial.result == "incorrect", 1L, 0L),
    Omission = fifelse(Trial.result == "omission", 1L, 0L)
  )]
  
  # Calculate summary statistics
  SWMData[, `:=`(
    TotalCorrect = sum(Correct, na.rm = TRUE),
    TotalIncorrect = sum(Incorrect, na.rm = TRUE),
    TotalOmissions = sum(Omission, na.rm = TRUE),
    MeanRT = mean(Response.time..ms., na.rm = TRUE),
    SDRT = sd(Response.time..ms., na.rm = TRUE)
  ), by = User.code]
  
  # Calculate strategy score
  SWMData[, Strategy := fifelse(
    mean(Response.time..ms., na.rm = TRUE) < 1000,
    "Fast", "Slow"
  ), by = User.code]
  
  return(SWMData)
} 