


deriveCUSPSURPS <- function(df, requiresReverseCoding = FALSE) {
  #Rotate
  df <- rotateQuestionnaire(df)
  if (requiresReverseCoding) {
    # reverse code
    reverseVariables <- c('A1_a$', 'A1_d$', 'A1_g$', 'A1_m$', 'A1_t$', 'A1_w$')
    df <- recodeVariables(df, reverseVariables, function(x) {
      5 - x
    })
  }
  #Summaries
  df$h_mean <-
    rowMeans(df[, grepl("A1_aR?$|A1_dR?$|A1_gR?$|A1_mR?$|A1_q|A1_tR?$|A1_wR?$",
                        colnames(df))])
  df$as_mean <-
    rowMeans(df[, grepl("A1_h|A1_j|A1_n|A1_r|A1_u", colnames(df))])
  df$imp_mean <-
    rowMeans(df[, grepl("A1_b$|A1_e|A1_k|A1_o|A1_v", colnames(df))])
  df$ss_mean <-
    rowMeans(df[, grepl("A1_c$|A1_f|A1_i|A1_l|A1_p|A1_s", colnames(df))])
  df$h_sum <-
    rowSums(df[, grepl("A1_aR?$|A1_dR?$|A1_gR?$|A1_mR?$|A1_q|A1_tR?$|A1_wR?$",
                       colnames(df))])
  df$as_sum <-
    rowSums(df[, grepl("A1_h|A1_j|A1_n|A1_r|A1_u", colnames(df))])
  df$imp_sum <-
    rowSums(df[, grepl("A1_b$|A1_e|A1_k|A1_o|A1_v", colnames(df))])
  df$ss_sum <-
    rowSums(df[, grepl("A1_c$|A1_f|A1_i|A1_l|A1_p|A1_s", colnames(df))])
  return(df)
}




Qs <- Psytools::downloadSingleDataFile(SMAusername,
                                       "CUSP",
                                       "CUSP_UBCO_DELTA_Q3-BASIC_DIGEST",
                                       "canada.psytools.com")
Qs <-
  rbind(Qs,
        selectIteration(
          Psytools::downloadSingleDataFile(
            SMAusername,
            "CUSP",
            "CUSP_Q3-BASIC_DIGEST",
            "canada.psytools.com"
          ),
          valid = FALSE
        ),
        fill = T)
Qs <-
  rbind(Qs,
        selectIteration(
          Psytools::downloadSingleDataFile(
            SMAusername,
            "CUSP",
            "CUSP_Q4-BASIC_DIGEST",
            "canada.psytools.com"
          ),
          valid = FALSE
        ),
        fill = T)
Qs <-
  rbind(Qs,
        selectIteration(
          Psytools::downloadSingleDataFile(
            SMAusername,
            "CUSP",
            "CUSP_ONTARIO_Q4-BASIC_DIGEST",
            "canada.psytools.com"
          ),
          valid = FALSE
        ),
        fill = T)
Qs <-
  rbind(Qs,
        selectIteration(
          Psytools::downloadSingleDataFile(
            SMAusername,
            "CUSP",
            "CUSP_OPfS_Q3-BASIC_DIGEST",
            "canada.psytools.com"
          ),
          valid = FALSE
        ),
        fill = T)

Qs <- Qs[User.code != 'DEV', ]
Qs <- Qs[!duplicated(subset(Qs, select = c(User.code, Iteration, Trial)), fromLast =
                       T), ]
# remove skip_back flags
Qs <- Qs[Qs$Trial.result != 'skip_back', ]
# remove js flags
Qs <- Qs[Qs$Block != 'js', ]
Qs <- rotateQuestionnaire(Qs)
setDT(Qs)
Qs <- Qs[registrationDF, on = c(User.code = "User.code")]
Qs <- Qs[!is.na(Iteration) &
           !grepl("TEST", name, ignore.case = T) , ]

## Q3 is now all sites, all data linked to registration for site info, schools in there for district info
Qs$District <- Qs$RegSample

Qs$District[Qs$schoolID %in% c('ndss', 'ss', 'sa', 'sdss', 'dl', 'dss', 'bu') &
              (Qs$RegSample == "CUSP_UBCO" |
                 Qs$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_DELTA"
Qs$District[Qs$schoolID %in% c('wss', 'skss', 'vss') &
              (Qs$RegSample == "CUSP_UBCO" |
                 Qs$RegSample == "OPfS_UBCO")] <- "CUSP_UBCO_KAMLOOPS"


Qs$SchoolYear[Qs$Processed.Timestamp > '2020-08-01'] <- "20-21"
Qs$SchoolYear[Qs$Processed.Timestamp > '2021-08-01'] <- "21-22"
Qs$SchoolYear[Qs$Processed.Timestamp > '2022-08-01'] <- "22-23"
Qs$SchoolYear[Qs$Processed.Timestamp > '2023-08-01'] <- "23-24"



keyQs <- c('J8', 'C1', 'G4', 'D1', 'D2', 'D3', 'D4', 'C2', 'District')
keyCols <- grep(paste(keyQs, collapse = "|"), names(Qs), value = T)
# for now limit to this last year and just UBCO and the key variables
#Qs<-Qs[RegSample=="CUSP_UBCO" & Processed.Timestamp > '2023-07-01',..keyCols ]

setDT(Qs)
UBCO_SUMMARY <- Qs[, list(
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









get_matching_ns <- function(.SD, min_days, max_days, max_dist = 0.20)
{
  # Subset based on strict criteria (date of birth, gender) and time window
  subset_data <- matching[User.code != .SD$User.code &
                            RegSample == .SD$RegSample &
                            #schoolID == .SD$schoolID &
                            (B1=="REFUSE" | .SD$B1 =="REFUSE" | B1 == .SD$B1) &
                            (B5=="REFUSE" | .SD$B5 =="REFUSE" | B5 == .SD$B5) &
                            as.numeric(date_of_testing - .SD$date_of_testing, units = "days") > min_days &
                            as.numeric(date_of_testing - .SD$date_of_testing, units = "days") < max_days]
  
  #flnames = strsplit(.SD$name, split= "/w+")
  #print(flnames[[1]])
  #print(flnames[[2]])
  # If the subset has more than one row, perform fuzzy matching
  if (nrow(subset_data) > 0) {
      print(paste(.SD$User.code, nrow(subset_data)))
    print(subset_data)
      return (as.character(subset_data[1, User.code]))
  
  } else {
    return (NA_character_)
  }
}

for (i in 1:nrow(ns_match)) {
  set(ns_match, i, "closest_duplicate", get_matching_ns(ns_match[i, ], -2, 2))
}
for (i in 1:nrow(ns_match)) {
  set(ns_match, i, "Y2match", get_matching_ns(ns_match[i, ], 60, 600))
}



# 2. Date discrepancies (matches more than 30 days from expected year intervals)
date_discrepancies <- get_date_discrepancies(matching, tolerance_days = 30)
write.xlsx(
  date_discrepancies,
  paste0("CUSP_DATE_DISCREPANCIES_", current_date, ".xlsx"),
  quote = FALSE,
  na = "",
  rowNames = FALSE
)