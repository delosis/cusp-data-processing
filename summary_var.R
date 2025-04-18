#' #Applying QC filters
#' ##CFT - There seems to be a significant year to year learning effect in this task
#' Percent of CFT records rejected by QC : `r round(nrow(subset(CFTData, CFTData$CFT_QC_CFT_ONE_RESPONSE==1))/ nrow(PALPData) * 100,digits=1)`%
# Set CFT output to NA according to the CFT_ONE_RESPONSE QC flag
CFTData[CFTData$CFT_QC_CFT_ONE_RESPONSE==1, grep("Test_1|Test_3|Raw_Total|ProRated_Total|IQ", names(CFTData)) ]<-NA

#' ##Palp - this is a LOT of exclusions - perhaps should be revised?
#' Percent of Palp records rejected by QC : `r round(nrow(subset(PALPData, PALPData$PALP_QC_PALP_WORSE_THAN_CHANCE ==1))/ nrow(PALPData) * 100,digits=1)`%
# Set PALP output to NA according to the PALP_WORSE_THAN_CHANCE QC flag - TOO FAST QC rule NOT applied
PALPData[PALPData$PALP_QC_PALP_WORSE_THAN_CHANCE==1, grep("Om|Com|Score|mean|Hits|CorrectNR|prime|bppd", names(PALPData)) ]<-NA

#' ##Depado Foil
QData$QC_DEPAPO_FOIL[!is.na(QData$DEPAPO_FOIL) & QData$DEPAPO_FOIL > 0 ]<-1
#' Percent of Depado Drug records rejected by FOIL QC : `r round(nrow(subset(QData, QData$QC_DEPAPO_FOIL==1))/ nrow(QData) * 100, digits= 1)`%
# Set DEPADO data to NA according to the DEPAPO_FOIL QC flag
QData[!is.na(QData$QC_DEPAPO_FOIL) & QData$QC_DEPAPO_FOIL ==1 , grep("^DEPAPO", names(QData)) ]<-NA

#' ##Depado >2.5 (from spss script filter - though this is commented as 5 in the script the calculation uses 2.5, please check which is correct!)
QData$QC_DEPAPO_UNCOMMON2.5[!is.na(QData$DEPAPO_TRANQ) & rowMeans(cbind(QData$DEPAPO_TRANQ,QData$DEPAPO_STEROIDS,QData$DEPAPO_SPEED,QData$DEPAPO_PCP,QData$DEPAPO_LSD,QData$DEPAPO_HEROIN,QData$DEPAPO_E),na.rm=T)>2.5]<-1
#' Percent of records rejected by >2.5 on uncommon drugs QC : `r round(nrow(subset(QData, QData$QC_DEPAPO_UNCOMMON2.5 == 1))/ nrow(QData) * 100, digits= 1)`%
# Set DEPADO data to NA according to the DEPAPO_UNCOMMON2.5 QC flag
#QData[!is.na(QData$QC_DEPAPO_UNCOMMON2.5) & QData$QC_DEPAPO_UNCOMMON2.5 == 1, grep("^DEPAPO", names(QData)) ]<-NA

#' ##Surps one answer
QData$QC_SURPS_ONE_ANSWER[rowMaxs(as.matrix(QData[, grep("^SURPS_[0-9]", names(QData)) ])) == rowMins(as.matrix(QData[, grep("^SURPS_[0-9]", names(QData)) ]))]<-1
#' Percent of records rejected by surps QC : `r round(nrow(subset(QData, QData$QC_SURPS_ONE_ANSWER==1))/ nrow(QData) * 100, digits= 1)`%
# Set SURPS data to NA according to the SURPS_ONE_ANSWER QC flag
QData[!is.na(QData$QC_SURPS_ONE_ANSWER) & QData$QC_SURPS_ONE_ANSWER==1, grep("^SURPS_", names(QData)) ]<-NA

#' ##SDQ one answer
QData$QC_SDQ_ONE_ANSWER[rowMaxs(as.matrix(QData[, grep("^SDQ_[0-9]", names(QData)) ])) == rowMins(as.matrix(QData[, grep("^SDQ_[0-9]", names(QData)) ]))]<-1
#' Percent of records rejected by SDQ QC : `r round(nrow(subset(QData, QData$QC_SDQ_ONE_ANSWER==1))/ nrow(QData) * 100, digits= 1)`%
# Set SDQ data to NA according to the SDQ_ONE_ANSWER QC flag
QData[!is.na(QData$QC_SDQ_ONE_ANSWER) & QData$QC_SDQ_ONE_ANSWER==1, grep("^SDQ_", names(QData)) ]<-NA






#' Create Summary variables for questionnaires
#' 
#' This function is applicable for all ??-Venture studies
#'
#' @param df Raw Wide format data frame
#' @keywords Data derivation 
summaryVarsQData<-function(df){
  # '' SURPS (Jacobs version)
  df$SURPS_NT<- df$SURPS_17 + 30 -df$SURPS_01 - df$SURPS_04 - df$SURPS_07 - df$SURPS_13 - df$SURPS_20 - df$SURPS_23
  df$SURPS_AS<- df$SURPS_08 + df$SURPS_10 + df$SURPS_14 + df$SURPS_18 + df$SURPS_21
  df$SURPS_IMP<- df$SURPS_02 + df$SURPS_05 + df$SURPS_11 + df$SURPS_15 + df$SURPS_22
  df$SURPS_SS<- df$SURPS_03 + df$SURPS_06 + df$SURPS_09 + df$SURPS_12 + df$SURPS_16 + df$SURPS_19
  # Z-Score by school
  # Split school code
  df <- separate(data = df,col = User.code,into = "School",sep = "_",remove = FALSE,extra = "drop")
  des_stat <- merge(aggregate(cbind(SURPS_NT,SURPS_AS,SURPS_IMP,SURPS_SS)~School, data=df, mean, na.rm=T),
                    aggregate(cbind(SURPS_NT,SURPS_AS,SURPS_IMP,SURPS_SS)~School, data=df, sd, na.rm=T), by="School",suffixes = c("_mean","_sd"))
  df <- merge(df,des_stat, by = "School", all.x=T)
  df$ZSURPS_NT_School <- ((df$SURPS_NT-df$SURPS_NT_mean)/df$SURPS_NT_sd) 
  df$ZSURPS_AS_School <- ((df$SURPS_AS-df$SURPS_AS_mean)/df$SURPS_AS_sd) 
  df$ZSURPS_IMP_School <- ((df$SURPS_IMP-df$SURPS_IMP_mean)/df$SURPS_IMP_sd) 
  df$ZSURPS_SS_School <- ((df$SURPS_SS-df$SURPS_SS_mean)/df$SURPS_SS_sd) 
  # Z-Score based on total sample
  df$ZSURPS_NT_Total <- ((df$SURPS_NT-mean(df$SURPS_NT,na.rm=TRUE))/sd(df$SURPS_NT,na.rm=TRUE)) 
  df$ZSURPS_AS_Total <- ((df$SURPS_AS-mean(df$SURPS_AS,na.rm=TRUE))/sd(df$SURPS_AS,na.rm=TRUE)) 
  df$ZSURPS_IMP_Total <- ((df$SURPS_IMP-mean(df$SURPS_IMP,na.rm=TRUE))/sd(df$SURPS_IMP,na.rm=TRUE)) 
  df$ZSURPS_SS_Total <- ((df$SURPS_SS-mean(df$SURPS_SS,na.rm=TRUE))/sd(df$SURPS_SS,na.rm=TRUE)) 
  df <- subset(df,select=-c(School,SURPS_NT_mean,SURPS_AS_mean,SURPS_IMP_mean,SURPS_SS_mean,SURPS_NT_sd,SURPS_AS_sd,SURPS_IMP_sd,SURPS_SS_sd))
  # SURPS GROUP
  df$max_Group_School <- apply(df[,grep("ZSURPS_NT_School|ZSURPS_AS_School|ZSURPS_IMP_School|ZSURPS_SS_School",names(df))],1,max)
  df$max_Group_Total <- apply(df[,grep("ZSURPS_NT_Total|ZSURPS_AS_Total|ZSURPS_IMP_Total|ZSURPS_SS_Total",names(df))],1,max)
  df$Group_School[df$ZSURPS_NT_School == df$max_Group_School & df$ZSURPS_NT_School > 1] <- "NT"
  df$Group_School[df$ZSURPS_AS_School == df$max_Group_School & df$ZSURPS_AS_School > 1] <- "AS"
  df$Group_School[df$ZSURPS_IMP_School == df$max_Group_School & df$ZSURPS_IMP_School > 1] <- "IMP"
  df$Group_School[df$ZSURPS_SS_School == df$max_Group_School & df$ZSURPS_SS_School > 1] <- "SS"
  df$Group_Total[df$ZSURPS_NT_Total == df$max_Group_Total & df$ZSURPS_NT_Total > 1] <- "NT"
  df$Group_Total[df$ZSURPS_AS_Total == df$max_Group_Total & df$ZSURPS_AS_Total > 1] <- "AS"
  df$Group_Total[df$ZSURPS_IMP_Total == df$max_Group_Total & df$ZSURPS_IMP_Total > 1] <- "IMP"
  df$Group_Total[df$ZSURPS_SS_Total == df$max_Group_Total & df$ZSURPS_SS_Total > 1] <- "SS"
  
  # '' SURPS (From Caculs_scores.sps)
  df$SURPS_echelle_TotalPenseesNegatives<- df$SURPS_17 + 30 -df$SURPS_01 - df$SURPS_04 - df$SURPS_07 - df$SURPS_13 - df$SURPS_20 - df$SURPS_23
  df$SURPS_echelle_TotalAnxiete<- df$SURPS_08 + df$SURPS_10 + df$SURPS_14 + df$SURPS_18 + df$SURPS_21
  df$SURPS_echelle_TotalImpulsivite<- df$SURPS_02 + df$SURPS_05 + df$SURPS_11 + df$SURPS_15 + df$SURPS_22
  df$SURPS_echelle_TotalSensationsFortes<- df$SURPS_03 + df$SURPS_06 + df$SURPS_09 + df$SURPS_12 + df$SURPS_16 + df$SURPS_19
  df$SURPS_echelle_TypologiePenseesNegatives<-recode(df$SURPS_echelle_TotalPenseesNegatives, "0:9=1;10:14=2;15:30=3")
  df$SURPS_echelle_TypologieAnxiete<-recode(df$SURPS_echelle_TotalAnxiete, "0:9=1;10:14=2;15:30=3")
  df$SURPS_echelle_TypologieImpulsivite<-recode(df$SURPS_echelle_TotalImpulsivite, "0:9=1;10:14=2;15:30=3")
  df$SURPS_echelle_TypologieSensationsFortes<-recode(df$SURPS_echelle_TotalSensationsFortes, "0:9=1;10:14=2;15:30=3")
  
  # ''Bullying - Not present in all studies
  if(length(grep('BULLY',names(df))) >0) {
    df$BULLY_echelle_bullied<-df$BULLY_01+df$BULLY_02+df$BULLY_03+df$BULLY_04+df$BULLY_11
    df$BULLY_echelle_bullier<-df$BULLY_05+df$BULLY_06+df$BULLY_07+df$BULLY_08+df$BULLY_12
    df$BULLY_Bullied<-0
    df$BULLY_Bullied[(df$BULLY_01>=2 | df$BULLY_02>=2 | df$BULLY_03>=2 | df$BULLY_04>=2)]<-1
  }
  # ''DEP_ADO
  if(length(grep('DEPAPO', names(df))) >0) {
  df$DEPAPO_ALC_WE2R <- recode(df$DEPAPO_ALC_WE2, "NA=0; 0:11=3; 12:15=2; else=1")
  df$DEPAPO_DRUG_WE2R <- recode(df$DEPAPO_DRUG_WE2, "NA=0; 0:13=3; 14:15=2; else=1")
  df$DEPAPO_DRUG_WE3R <- recode(df$DEPAPO_DRUG_WE3, "NA=0; 1=8; 0=0")
  df$DEPAPO_OC3R <- 0
  df$DEPAPO_OC3R[df$DEPAPO_ALC_OC3==1 | df$DEPAPO_DRUG_OC3==1] <- 1
  df$DEPAPO_ALC_OC5R <- recode(df$DEPAPO_ALC_OC5, "NA=0; 0=0; 1:2=1; 3:25=2; else=3")
  df$DEPAPO_ALC_OC6R <- recode(df$DEPAPO_ALC_OC6, "NA=0; 0=0; 1:2=1; 3:25=2; else=3")
  df[, grepl("DEPAPO_(DRUG|ALC)_HARM", names(df))]<-na.zero(df[, grepl("DEPAPO_(DRUG|ALC)_HARM", names(df))])
  df$DEPAPO_HARM01R[df$DEPAPO_ALC_HARM01==1 | df$DEPAPO_DRUG_HARM01==1] <- 1
  df$DEPAPO_HARM02R[df$DEPAPO_ALC_HARM02==1 | df$DEPAPO_DRUG_HARM02==1] <- 1
  df$DEPAPO_HARM03R[df$DEPAPO_ALC_HARM03==1 | df$DEPAPO_DRUG_HARM03==1] <- 1
  df$DEPAPO_HARM04R[df$DEPAPO_ALC_HARM04==1 | df$DEPAPO_DRUG_HARM04==1] <- 1
  df$DEPAPO_HARM05R[df$DEPAPO_ALC_HARM05==1 | df$DEPAPO_DRUG_HARM05==1] <- 1
  df$DEPAPO_HARM06R[df$DEPAPO_ALC_HARM06==1 | df$DEPAPO_DRUG_HARM06==1] <- 1
  df$DEPAPO_HARM07R[df$DEPAPO_ALC_HARM07==1 | df$DEPAPO_DRUG_HARM07==1] <- 1
  df$DEPAPO_HARM08R[df$DEPAPO_ALC_HARM08==1 | df$DEPAPO_DRUG_HARM08==1] <- 1
  df$DEPAPO_HARM09R[df$DEPAPO_ALC_HARM09==1 | df$DEPAPO_DRUG_HARM09==1] <- 1
  df$DEPAPO_HARM10R[df$DEPAPO_ALC_HARM10==1 | df$DEPAPO_DRUG_HARM10==1] <- 1
  df[, grepl("DEPAPO_HARM", names(df))]<-na.zero(df[, grepl("DEPAPO_HARM", names(df))])
  df$DEPAPO_ECHELLE_FACTEUR1_SOMME <-
    ifelse(
      df$DEM_01 == 1,
      df$DEPAPO_ALC + df$DEPAPO_HASH + df$DEPAPO_ALC_WE2R + df$DEPAPO_DRUG_WE2R + df$DEPAPO_OC3R + df$DEPAPO_ALC_OC6R + df$DEPAPO_HARM09R,
      df$DEPAPO_ALC + df$DEPAPO_HASH + df$DEPAPO_ALC_WE2R + df$DEPAPO_ALC_WE2R + df$DEPAPO_DRUG_WE2R + df$DEPAPO_OC3R + df$DEPAPO_ALC_OC5R + df$DEPAPO_HARM09R
    )
  df$DEPAPO_ECHELLE_FACTEUR2_SOMME <- rowSums(df[,grep("^(DEPAPO_COKE|DEPAPO_GLUE|DEPAPO_LSD|DEPAPO_PCP|DEPAPO_HEROIN|DEPAPO_SPEED|DEPAPO_OPIATE|DEPAPO_E|DEPAPO_FOIL|DEPAPO_PRESCRIPTION|DEPAPO_TRANQ|DEPAPO_STEROIDS|DEPAPO_OTHER|DEPAPO_DRUG_WE3R)$", names(df))])
  df$DEPAPO_ECHELLE_FACTEUR3_SOMME <- df$DEPAPO_HARM02R + df$DEPAPO_HARM03R + df$DEPAPO_HARM04R + df$DEPAPO_HARM05R + df$DEPAPO_HARM07R + df$DEPAPO_HARM10R
  df$DEPAPO_ECHELLE_TOTAL <- df$DEPAPO_ECHELLE_FACTEUR1_SOMME + df$DEPAPO_ECHELLE_FACTEUR2_SOMME + df$DEPAPO_ECHELLE_FACTEUR3_SOMME
  df$DEPAPO_ECHELLE_LUMIERE <- recode(df$DEPAPO_ECHELLE_TOTAL ,"NA=NA;0:13=1;14:19=2;else=3")
  df$DEPAPO_Conseq_alcohol <- df$DEPAPO_ALC_HARM02 + df$DEPAPO_ALC_HARM03 + df$DEPAPO_ALC_HARM04 +df$DEPAPO_ALC_HARM05 + df$DEPAPO_ALC_HARM07 + df$DEPAPO_ALC_HARM10
  df$DEPAPO_Conseq_drug <- df$DEPAPO_DRUG_HARM02 + df$DEPAPO_DRUG_HARM03 + df$DEPAPO_DRUG_HARM04 +df$DEPAPO_DRUG_HARM05 + df$DEPAPO_DRUG_HARM07 + df$DEPAPO_DRUG_HARM10
  }
  
  # '' BSI - Not in all studies
  if ("BSI_01" %in% colnames(df)){
    df$BSI_echelle_TotalDepression <- df$BSI_01 + df$BSI_02 + df$BSI_03 + df$BSI_04 + df$BSI_05 + df$BSI_06 + df$BSI_07
    df$BSI_echelle_TotalAnxiete <- df$BSI_08 + df$BSI_09 + df$BSI_10 + df$BSI_11 + df$BSI_12
    df$BSI_echelle_PenseesSuicidaire <- df$BSI_01
  }
  # '' SDQ - Not in All studies
  if ("SDQ_03" %in% colnames(df)){
    df<-cbind(df,SDQSummaries(df))
  }
  # '' Psychotic
  df$PSYCHOTIC_Scoretotal<- df$PSYCHOTIC_01 + df$PSYCHOTIC_02 + df$PSYCHOTIC_03 + df$PSYCHOTIC_04 + 
  df$PSYCHOTIC_05 + df$PSYCHOTIC_08 + df$PSYCHOTIC_09
  df$PSYCHOTIC_Abn<-0
  df$PSYCHOTIC_Abn[(df$PSYCHOTIC_Scoretotal>=4 & df$PSYCHOTIC_04>=1)]<-1
  
  # '' APQ - Full spec only found in Interventure Pilot
  if ("APQ_01" %in% colnames(df)){
    df$APQ_Echelle_involvement<-df$APQ_01+df$APQ_04+df$APQ_07+df$APQ_09+df$APQ_11+df$APQ_14+df$APQ_15+df$APQ_20+df$APQ_23+df$APQ_26
    df$APQ_Echelle_Positive<-df$APQ_02+df$APQ_05+df$APQ_13+df$APQ_18+df$APQ_27
    df$APQ_Echelle_Monitoring<-df$APQ_06+df$APQ_10+df$APQ_17+df$APQ_19+df$APQ_21+df$APQ_24+df$APQ_28+df$APQ_29+df$APQ_30+df$APQ_32
    df$APQ_Echelle_Discipline<-df$APQ_03+df$APQ_08+df$APQ_12+df$APQ_22+df$APQ_25+df$APQ_31
    df$APQ_Echelle_Corporal<-df$APQ_33+df$APQ_35+df$APQ_39+df$APQ_22+df$APQ_25+df$APQ_31
    df$APQ_Echelle_Total<-df$APQ_Echelle_involvement+df$APQ_Echelle_Positive+df$APQ_Echelle_Monitoring+df$APQ_Echelle_Discipline+df$APQ_Echelle_Corporal
  }else {

        # '' APQ - partial score used in interventure main
    if ("APQ_04A" %in% colnames(df)){
      df$APQ_Dysfunctional_Parenting_Composite <- (6-(df$APQ_04+df$APQ_04A)/2) +
                                                  df$APQ_08 +
                                                  df$APQ_10 +
                                                  df$APQ_12 +
                                                  (6-df$APQ_13) +
                                                  df$APQ_17 +
                                                  df$APQ_19 +
                                                  (6-(df$APQ_20+df$APQ_20A)/2) +
                                                  df$APQ_31 +
                                                  df$APQ_39
      df$APQ_Dysfunctional_Parenting_Composite_High<-ifelse(df$APQ_Dysfunctional_Parenting_Composite > quantile(df$APQ_Dysfunctional_Parenting_Composite, 0.9, names=FALSE, na.rm=TRUE), 1,0)
     }
  }
  
  # '' SELF-ESTEEM (All recoded -1 : 3,5,8,9,10 reversed)
  if ("SELF_ESTEEM_01" %in% colnames(df)){
    df$SELF_ESTEEM_Echelle_Total<- -10+df$SELF_ESTEEM_01+df$SELF_ESTEEM_02+df$SELF_ESTEEM_04+df$SELF_ESTEEM_06+df$SELF_ESTEEM_07-10+
      15-df$SELF_ESTEEM_03-df$SELF_ESTEEM_05-df$SELF_ESTEEM_08-df$SELF_ESTEEM_09-df$SELF_ESTEEM_10
  }
  # '' DMQ
  if ("DMQ_01" %in% colnames(df)){
    df$DMQ_Echelle_Social_Total<-df$DMQ_01+df$DMQ_04+df$DMQ_07+df$DMQ_10+df$DMQ_13
    df$DMQ_Echelle_Anxiety_Total<-df$DMQ_02+df$DMQ_08+df$DMQ_11+df$DMQ_19
    df$DMQ_Echelle_Depression_Total<-df$DMQ_05+df$DMQ_14+df$DMQ_16+df$DMQ_17+df$DMQ_20+df$DMQ_21+df$DMQ_22+df$DMQ_23+df$DMQ_27
    df$DMQ_Echelle_Ehancement_Total<-df$DMQ_03+df$DMQ_06+df$DMQ_09+df$DMQ_12+df$DMQ_26
    df$DMQ_Echelle_Conformity_Total<-df$DMQ_15+df$DMQ_18+df$DMQ_24+df$DMQ_25+df$DMQ_28
  }
  # '' CMQ
  if ("CMQ_01" %in% colnames(df)){
    df$CMQ_Echelle_Social_Total<-df$CMQ_01+df$CMQ_04+df$CMQ_07+df$CMQ_10+df$CMQ_13
    df$CMQ_Echelle_Anxiety_Total<-df$CMQ_02+df$CMQ_08+df$CMQ_11+df$CMQ_19
    df$CMQ_Echelle_Depression_Total<-df$CMQ_05+df$CMQ_14+df$CMQ_16+df$CMQ_17+df$CMQ_20+df$CMQ_21+df$CMQ_22+df$CMQ_23+df$CMQ_27
    df$CMQ_Echelle_Ehancement_Total<-df$CMQ_03+df$CMQ_06+df$CMQ_09+df$CMQ_12+df$CMQ_26
    df$CMQ_Echelle_Conformity_Total<-df$CMQ_15+df$CMQ_18+df$CMQ_24+df$CMQ_25+df$CMQ_28
  }
  
  # '' HQA
  if ("HQA_01" %in% colnames(df)){
    df$HQA_MajorDepressiveEpisode <-
      ifelse(((df$HQA_01 == 2 | df$HQA_02 == 2) &
                rowSums(
                  cbind(
                    df$HQA_01 == 2,
                    df$HQA_02 == 2,
                    df$HQA_03 == 2,
                    df$HQA_04 == 2,
                    df$HQA_05 == 2,
                    df$HQA_06 == 2,
                    df$HQA_07 == 2,
                    df$HQA_08 == 2
                  ),
                  na.rm = T
                ) > 4 &
                df$HQA_15 > 2
      ), 1, 0)
    df$HQA_Depression <-
      ifelse(((df$HQA_01 > 0 | df$HQA_02 >0) &
                rowSums(
                  cbind(
                    df$HQA_01,
                    df$HQA_02,
                    df$HQA_03,
                    df$HQA_04,
                    df$HQA_05,
                    df$HQA_06,
                    df$HQA_07,
                    df$HQA_08
                  ),
                  na.rm = T
                ) > 4 &
                df$HQA_15 > 1
      ), 1, 0)
    # 'Note the options for HQA_14 changed from yes no to a/b/c/d/e in October 2017
    # 'This works for either
    df$HQA_SuicidalIdeation <-
      ifelse((df$HQA_13 == 1 & (df$HQA_14 == 'd' | df$HQA_14 =='e' | df$HQA_14 ==1)), 1, 0)
    df$HQA_Anxiety <-
      ifelse(((df$HQA_32 == 1 & df$HQA_33 == 1 & df$HQA_34 == 1) &
                (
                  df$HQA_35 + df$HQA_36 + df$HQA_37 + df$HQA_38 + df$HQA_39 + df$HQA_40
                ) > 2 &
                rowSums(
                  cbind(
                    df$HQA_35 == 2,
                    df$HQA_36 == 2,
                    df$HQA_37 == 2,
                    df$HQA_38 == 2,
                    df$HQA_39 == 2,
                    df$HQA_40 == 2
                  ),
                  na.rm = T
                ) > 0 & df$HQA_41 > 2
      ), 1, 0)
    df$FAST_TRACK_1<-ifelse(df$HQA_Depression & df$HQA_SuicidalIdeation, 1,0)
    df$FAST_TRACK_2<-ifelse(df$HQA_SuicidalIdeation & df$HQA_Anxiety, 1,0)
    if ("DEPAPO_ECHELLE_LUMIERE" %in% colnames(df)){
    df$FAST_TRACK_3<-ifelse(df$HQA_SuicidalIdeation & df$DEPAPO_ECHELLE_LUMIERE==2, 1,0)
    }
    if ("SDQ_03" %in% colnames(df)){
      df$FAST_TRACK_4<-ifelse(df$HQA_SuicidalIdeation & df$SDQ_echelle_ConductProb_Abnormal & df$SDQ_echelle_Impact_Abnormal, 1,0)
    }
    if ("BULLY_01" %in% colnames(df)){
      df$FAST_TRACK_5<-ifelse(df$HQA_SuicidalIdeation & df$BULLY_Bullied, 1,0)
    }
    if ("DEPAPO_ECHELLE_LUMIERE" %in% colnames(df)){
    df$FAST_TRACK_6<-ifelse(df$DEPAPO_ECHELLE_LUMIERE==2 | (!is.na(df$DEPAPO_DRUG_WE3) & df$DEPAPO_DRUG_WE3==1) , 1,0)
    }
  }
  
  

  
  # '' COPE
  if ("COPE_01" %in% colnames(df)){
    df$COPE_Echelle_SelfDistraction<-df$COPE_01+df$COPE_17
    df$COPE_Echelle_ActiveCoping<-df$COPE_02+df$COPE_20
    df$COPE_Echelle_Denial<-df$COPE_03+df$COPE_21
    df$COPE_Echelle_SubstanceUse<-df$COPE_04+df$COPE_22
    df$COPE_Echelle_EmotionalSupport<-df$COPE_05+df$COPE_14
    df$COPE_Echelle_InstrumentalSupport<-df$COPE_10+df$COPE_19
    df$COPE_Echelle_BehDis<-df$COPE_06+df$COPE_15
    df$COPE_Echelle_Venting<-df$COPE_09+df$COPE_18
    df$COPE_Echelle_PositiveReframing<-df$COPE_11+df$COPE_26
    df$COPE_Echelle_Planning<-df$COPE_13+df$COPE_24
    df$COPE_Echelle_Humor<-df$COPE_16+df$COPE_28
    df$COPE_Echelle_Acceptance<-df$COPE_08+df$COPE_23
    df$COPE_Echelle_Religion<-df$COPE_07+df$COPE_27
    df$COPE_Echelle_SelfBlame<-df$COPE_12+df$COPE_25
  }
  
  return(df)
}


SDQSummaries<-function(sdqData) {
  df<-subset(sdqData,select=c(SDQ_03,SDQ_08,SDQ_13,SDQ_16,SDQ_24))
  SDQ_echelle_emotion_prob<-round(rowMeans(data.matrix(df), na.rm=TRUE) * 5)
  SDQ_echelle_emotion_prob[rowSums(is.na(df))>2]<-NA
  
  df<-subset(sdqData,select=c(SDQ_05,SDQ_07,SDQ_12,SDQ_18,SDQ_22))
  df$SDQ_07<-recode(df$SDQ_07, "2=0;0=2")
  SDQ_echelle_conduct_prob<-round(rowMeans(data.matrix(df), na.rm=TRUE) * 5)
  SDQ_echelle_conduct_prob[rowSums(is.na(df))>2]<-NA
  
  df<-subset(sdqData,select=c(SDQ_02, SDQ_10,SDQ_15,SDQ_21,SDQ_25))
  df$SDQ_21<-recode(df$SDQ_21, "2=0;0=2")
  df$SDQ_25<-recode(df$SDQ_25, "2=0;0=2")
  SDQ_echelle_hyper_prob<-round(rowMeans(data.matrix(df), na.rm=TRUE) * 5)
  SDQ_echelle_hyper_prob[rowSums(is.na(df))>2]<-NA
  
  df<-subset(sdqData,select=c(SDQ_06,SDQ_11, SDQ_14, SDQ_19, SDQ_23))
  df$SDQ_11<-recode(df$SDQ_11, "2=0;0=2")
  df$SDQ_14<-recode(df$SDQ_14, "2=0;0=2")
  SDQ_echelle_peer_prob<-round(rowMeans(data.matrix(df), na.rm=TRUE) * 5)
  SDQ_echelle_peer_prob[rowSums(is.na(df))>2]<-NA
  
  df<-subset(sdqData,select=c(SDQ_01,SDQ_04,SDQ_09,SDQ_17,SDQ_20))
  SDQ_echelle_prosoc_beh<-round(rowMeans(data.matrix(df), na.rm=TRUE) * 5)
  SDQ_echelle_prosoc_beh[rowSums(is.na(df))>2]<-NA
  
  # '' IMPACT
  if ("SDQ_29" %in% colnames(sdqData)){
    df<-subset(sdqData,select=c(SDQ_27,SDQ_28,SDQ_29,SDQ_30,SDQ_31,SDQ_32,SDQ_33))
    df$SDQ_29<-recode(df$SDQ_29, "2=1;3=2;else=0")
    df$SDQ_30<-recode(df$SDQ_30, "2=1;3=2;else=0")
    df$SDQ_31<-recode(df$SDQ_31, "2=1;3=2;else=0")
    df$SDQ_32<-recode(df$SDQ_32, "2=1;3=2;else=0")
    df$SDQ_33<-recode(df$SDQ_33, "2=1;3=2;else=0")
    SDQ_impact<-df$SDQ_30 + df$SDQ_31 + df$SDQ_32 + df$SDQ_33
  }
  
  df<-data.frame(SDQ_echelle_emotion_prob,SDQ_echelle_conduct_prob,SDQ_echelle_hyper_prob,SDQ_echelle_peer_prob)
  df$SDQ_echelle_total_difficulties<-rowSums(data.matrix(df), na.rm=TRUE)
  df$SDQ_echelle_total_difficulties[rowSums(is.na(df))>0]<-NA
  
  df<-cbind(df,SDQ_echelle_prosoc_beh)
  
  if ("SDQ_29" %in% colnames(sdqData)){
    df<-cbind(df,SDQ_impact)
    df$SDQ_echelle_Impact_Abnormal <- recode(df$SDQ_impact, "0:1=0;2:10=1")
    
  }
  
  df$SDQ_echelle_Hyper_Band <- recode(df$SDQ_echelle_hyper_prob, "0:5=0;6=1;7:10=2")
  df$SDQ_echelle_EmotionProb_Band <- recode(df$SDQ_echelle_emotion_prob, "0:5=0;6=1;7:10=2")
  df$SDQ_echelle_ConductProb_Band <- recode(df$SDQ_echelle_conduct_prob, "0:5=0;6=1;7:10=2")
  df$SDQ_echelle_PeerProb_Band <- recode(df$SDQ_echelle_peer_prob, "0:5=0;6=1;7:10=2")
  df$SDQ_echelle_ProsocProb_Band <- recode(df$SDQ_echelle_prosoc_beh, "0:4=2;6:10=0;5=1")
  df$SDQ_echelle_TotalDiff_Band <- recode(df$SDQ_echelle_total_difficulties, "0:15=0;16:19=1;20:40=2")
  
  df$SDQ_echelle_TotalDiff_Abnormal <- recode(df$SDQ_echelle_total_difficulties, "0:19=0;20:40=1")
  df$SDQ_echelle_TotalDiff_Borderline <- recode(df$SDQ_echelle_total_difficulties, "0:15=0;16:40=1")
  df$SDQ_echelle_EmotionProb_Borderline <- recode(df$SDQ_echelle_emotion_prob, "0:5=0;6:10=1")
  df$SDQ_echelle_ConductProb_Borderline <- recode(df$SDQ_echelle_conduct_prob, "0:3=0;4:10=1")
  df$SDQ_echelle_HyperProb_Borderline <- recode(df$SDQ_echelle_hyper_prob, "0:5=0;6:10=1")
  df$SDQ_echelle_PeerProb_Borderline <- recode(df$SDQ_echelle_peer_prob, "0:3=0;4:10=1")
  df$SDQ_echelle_EmotionProb_Abnormal <- recode(df$SDQ_echelle_emotion_prob, "0:6=0;7:10=1")
  df$SDQ_echelle_ConductProb_Abnormal <- recode(df$SDQ_echelle_conduct_prob, "0:4=0;5:10=1")
  df$SDQ_echelle_HyperProb_Abnormal <- recode(df$SDQ_echelle_hyper_prob, "0:6=0;7:10=1")
  df$SDQ_echelle_PeerProb_Abnormal <- recode(df$SDQ_echelle_peer_prob, "0:5=0;6:10=1")
  
  return(df)
}

#' CMSSummary data
#' Generates the summary vriables for CMS task
#' @param CMSData dataframe containing the long format CMS data
#' @keywords download dataset
summaryVarsCMS<-function(CMSData){
  # Score
  CMSData$Trial.result <- sub(pattern = "\\|", replacement = "_",x = CMSData$Trial.result)
  CMSData <- separate(CMSData,col = Trial.result,sep = "_",into = c("Score","Trial.result"))
  # Trial to variables
  CMSData <- subset(CMSData, select=c(User.code, Version, Trial,Score))
  CMSData <- spread(CMSData,key = Trial,value = Score,convert = TRUE)
}

#' SWMSummary data
#' Generates the summary variables for SWM task
#' NB This could be extended to provide a Strategy score like the cantab version if desired
#' @param SWMData dataframe containing the long format SWM data
#' @keywords download dataset
summaryVarsSWM<-function(SWMData){
  ## Fixed Jacob's function and incorporated here
  f2 <-   function(x) { 
    runs <- rle(x == 0L) 
    with(runs, max(lengths[values])) 
  }
  lgtsp <- SWMData %>% select(User.code,Block_Trial=Block,Between.search.errors) %>%  separate(Block_Trial,c("Block","Trial"),remove = F) %>% mutate(ID.Block=paste(User.code,Block_Trial,sep="."))
  lgtsp <- with(lgtsp, tapply(Between.search.errors, ID.Block, f2))
  lgtsp <- data.frame(lgtsp)
  lgtsp$lgtsp <- as.numeric(lgtsp$lgtsp)
  lgtsp$ID <- as.character(rownames(lgtsp))
  lgtsp$ID <- gsub("_1$|_2$","",lgtsp$ID)
  lgtsp <- lgtsp %>% group_by(ID) %>% summarise_each(funs(mean)) %>% separate(ID,c("ID","Block"), sep = "\\.") %>% spread(Block,lgtsp,convert=T) %>% select(ID,t4_span=`4`,t6_span=`6`,t8_span=`8`,t10_span=`10`)
  SWMData <- aggregate(cbind(Between.search.errors,Within.search.errors)~User.code+Block, data=SWMData, sum, na.rm=T)
  names(SWMData)<-c("User.code", "Block", "BSE", "WSE")
  SWMData<-dcast(melt(SWMData, id.vars=1:2), User.code ~ variable+Block)
  SWMData$BSE_TOTAL_8 <- SWMData$BSE_4_1 + SWMData$BSE_4_2 + SWMData$BSE_6_1 + SWMData$BSE_6_2 +SWMData$BSE_8_1 + SWMData$BSE_8_2
  SWMData$WSE_TOTAL_8 <- SWMData$WSE_4_1 + SWMData$WSE_4_2 + SWMData$WSE_6_1 + SWMData$WSE_6_2 +SWMData$WSE_8_1 + SWMData$WSE_8_2
  SWMData$BSE_TOTAL_10 <- SWMData$BSE_TOTAL_8 + SWMData$BSE_10_1 + SWMData$BSE_10_2
  SWMData$WSE_TOTAL_10 <- SWMData$WSE_TOTAL_8 + SWMData$WSE_10_1 + SWMData$WSE_10_2
  SWMData<-merge(SWMData, lgtsp, by.x="User.code", by.y="ID")
  return(SWMData)
}


#' DPSummary data
#' Generates the summary variables for DP task
#' @param DPData dataframe containing the long format DP data
#' @keywords download dataset
summaryVarsDP<-function(DPData){
  DPData<-data.table(DPData, do.call("rbind", strsplit(DPData$Trial, '_')))
  DPData$Emotion<-recode(DPData$V1, "'t'='threat';'f'='fear';'h'='happy';'n'='neutral'")
  DPData$Congruence<-tolower(DPData$V2)
  DPData<-subset(DPData, Trial.result =='PASS')
  DPData$Response.time..ms.<-as.numeric(DPData$Response.time..ms.)
  DPData <-do.call(data.frame,aggregate(Response.time..ms.~User.code+Completed+Emotion+Congruence, data=DPData, FUN=function(x) c(RT = mean(x),hits = length(x))))
  DPData$User.code<-as.character(DPData$User.code)
  names(DPData)<-c("User.code", "Completed", "Emotion", "Congruence", "RT", "hits")
  DPData<-dcast(melt(DPData, id.vars=1:4), User.code+Completed ~ variable+Emotion+Congruence)
  DPData$RT_happy_diff <- DPData$RT_happy_i - DPData$RT_happy_c
  DPData$RT_fear_diff <- DPData$RT_fear_i - DPData$RT_fear_c
  DPData$RT_threat_diff <- DPData$RT_threat_i - DPData$RT_threat_c
  DPData$RT_all_diff <- (DPData$RT_happy_i + DPData$RT_fear_i + DPData$RT_threat_i)/4 - 
    (DPData$RT_happy_c + DPData$RT_fear_c + DPData$RT_threat_c)/4 
  
  return(DPData)
}


#' CFTSummary data
#' Generates the summary variables for CFT task
#' An Ages df must be supplied with the ages in years for each User.code
#' This will produce no estimate for any participant who does not finish both tests
#' It would be possible to modify to prorate from the first test for the ~50 who do not complete both
#' @param CFTData dataframe containing the long format CFT data
#' @keywords download dataset
summaryVarsCFT<-function(CFTData, Ages){
  #remove any incomplete
  CFTData<-subset(CFTData, Completed=='t')
  
  #Recode Pass / Fail to numeric
  CFTData$Trial.result <- Recode(var = CFTData$Trial.result,"'PASS'=1; 'FAIL'=0")
  
  #Create QC flag
  CFTDataQC<-merge(aggregate(Response~User.code+Block, data=CFTData, max, na.rm=T),
                   aggregate(Response~User.code+Block, data=CFTData, min, na.rm=T), by=c("User.code", "Block"))
  CFTDataQC$QC_CFT_ONE_RESPONSE<-ifelse(CFTDataQC$Response.x ==CFTDataQC$Response.y,1,0)
  CFTDataQC <- aggregate(QC_CFT_ONE_RESPONSE~User.code, data=CFTDataQC, max, na.rm=T)
  
  #Block Totals
  CFTData <- aggregate(Trial.result~User.code+Block, data=CFTData, sum, na.rm=T)
  names(CFTData)<-c("User.code", "Block", "Total")
  
  CFTData <- spread(CFTData,key = Block,value = Total,convert = TRUE)
  CFTData$Raw_Total<-CFTData$Test_1 + CFTData$Test_3
  CFTData$ProRated_Total<-(((CFTData$Raw_Total)/23)*44)
  
  CFTData<-merge(CFTData, Ages, by="User.code", all.x=T)
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total<= 14.5] <- 57
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 62
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 66
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 70
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 73
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 76
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 79
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 81
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 83
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 84
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 86
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 87
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 89
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 91
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 92
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 94
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 96
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 97
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 99
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 102
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 104.5
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 109
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 113
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 118
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 122
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 127
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 133
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 139
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 41.5 & CFTData$ProRated_Total <=42.5] <- 145
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 42.5 & CFTData$ProRated_Total <=43.5] <- 151
  CFTData$IQ[CFTData$AgeAtTesting >13.75 & CFTData$ProRated_Total > 43.5 ] <- 15
  
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total<= 14.5] <- 59
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 63
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 68
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 71
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 74
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 77
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 80
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 82
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 84
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 86
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 88
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 89
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 91
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 93
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 96
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 98
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 100
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 103
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 106
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 108.5
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 112
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 116
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 120
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 124
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 130
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 136
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 142
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 41.5 & CFTData$ProRated_Total <=42.5] <- 146
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 42.5 & CFTData$ProRated_Total <=43.5] <- 152
  CFTData$IQ[CFTData$AgeAtTesting >13.25 & CFTData$ProRated_Total > 43.5 ] <- 160
  
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total<= 14.5] <- 61
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 65
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 69
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 72
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 76
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 78
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 81
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 83
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 85
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 87
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 89
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 91
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 93
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 97
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 99
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 101
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 103
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 105
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 108
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 110.5
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 115
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 118
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 122
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 126
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 132
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 138
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 143
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 41.5 & CFTData$ProRated_Total <=42.5] <- 148
  CFTData$IQ[CFTData$AgeAtTesting >12.75 & CFTData$ProRated_Total > 42.5] <- 153
  
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total<= 13.5] <- 59
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 13.5 & CFTData$ProRated_Total <=14.5] <- 64
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 68
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 71
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 74
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 77
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 79
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 82
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 84
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 86
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 89
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 91
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 93
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 98
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 100
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 102
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 104
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 106
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 109
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 111
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 113.5
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 118
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 122
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 125
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 129
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 134
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 140
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 144
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 41.5 & CFTData$ProRated_Total <=42.5] <- 149
  CFTData$IQ[CFTData$AgeAtTesting >12.25 & CFTData$ProRated_Total > 42.5] <- 155
  
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total<= 12.5] <- 59
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 12.5 & CFTData$ProRated_Total <=13.5] <- 64
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 13.5 & CFTData$ProRated_Total <=14.5] <- 68
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 71
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 74
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 76
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 79
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 81
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 84
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 86
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 88
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 91
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 93
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 97
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 100
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 102
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 104
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 107
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 109
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 112
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 115
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 117.5
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 121
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 125
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 128
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 132
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 135
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 142
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 145
  CFTData$IQ[CFTData$AgeAtTesting >11.75 & CFTData$ProRated_Total > 41.5 ] <- 151
  
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total<= 12.5] <- 62
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 12.5 & CFTData$ProRated_Total <=13.5] <- 66
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 13.5 & CFTData$ProRated_Total <=14.5] <- 69
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 72
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 75
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 78
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 80
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 82
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 85
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 87
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 90
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 93
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 97
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 100
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 103
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 105
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 108
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 110
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 113
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 116
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 119
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 121.5
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 125
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 128
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 131
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 135
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 138
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 143
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 147
  CFTData$IQ[CFTData$AgeAtTesting >11.25 & CFTData$ProRated_Total > 41.5 ] <- 152
  
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total<= 11.5] <- 60
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 11.5 & CFTData$ProRated_Total <=12.5] <- 64
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 12.5 & CFTData$ProRated_Total <=13.5] <- 68
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 13.5 & CFTData$ProRated_Total <=14.5] <- 71
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 14.5 & CFTData$ProRated_Total <=15.5] <- 73
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 15.5 & CFTData$ProRated_Total <=16.5] <- 76
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 16.5 & CFTData$ProRated_Total <=17.5] <- 79
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 17.5 & CFTData$ProRated_Total <=18.5] <- 81
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 18.5 & CFTData$ProRated_Total <=19.5] <- 84
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 19.5 & CFTData$ProRated_Total <=20.5] <- 87
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 20.5 & CFTData$ProRated_Total <=21.5] <- 89
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 21.5 & CFTData$ProRated_Total <=22.5] <- 92
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 22.5 & CFTData$ProRated_Total <=23.5] <- 95
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 23.5 & CFTData$ProRated_Total <=24.5] <- 97
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 24.5 & CFTData$ProRated_Total <=25.5] <- 100
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 25.5 & CFTData$ProRated_Total <=26.5] <- 103
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 26.5 & CFTData$ProRated_Total <=27.5] <- 105
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 27.5 & CFTData$ProRated_Total <=28.5] <- 108
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 28.5 & CFTData$ProRated_Total <=29.5] <- 111
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 29.5 & CFTData$ProRated_Total <=30.5] <- 113
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 30.5 & CFTData$ProRated_Total <=31.5] <- 116
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 31.5 & CFTData$ProRated_Total <=32.5] <- 119
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 32.5 & CFTData$ProRated_Total <=33.5] <- 121
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 33.5 & CFTData$ProRated_Total <=34.5] <- 123.5
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 34.5 & CFTData$ProRated_Total <=35.5] <- 127
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 35.5 & CFTData$ProRated_Total <=36.5] <- 130
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 36.5 & CFTData$ProRated_Total <=37.5] <- 133
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 37.5 & CFTData$ProRated_Total <=38.5] <- 136
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 38.5 & CFTData$ProRated_Total <=39.5] <- 139
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 39.5 & CFTData$ProRated_Total <=40.5] <- 144
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 40.5 & CFTData$ProRated_Total <=41.5] <- 148
  CFTData$IQ[CFTData$AgeAtTesting >10.75 & CFTData$ProRated_Total > 41.5 ] <- 153
  
  #Drop the age at testing, add QC and return
  CFTData<-subset(merge(CFTData, CFTDataQC, by="User.code", all.x=T), select=-AgeAtTesting)
  return(CFTData)
}

#' PALP Summary data
#' Generates the summary vriables for PALP task
#' @param PALPdata dataframe containing the long format PALP data
#' @keywords download dataset
summaryVarsPALP<-function(PALPData){
  # Scores
  PALPData$Om <- ifelse((!grepl("space",PALPData$Response) & grepl("^I",PALPData$Trial.result)),yes = 1,no = 0 )
  PALPData$Com <- ifelse((grepl("space",PALPData$Response) & grepl("^I",PALPData$Trial.result)),yes = 1,no = 0 )
  PALPData$condition[grepl("RR",PALPData$Block)] <- "RR" 
  PALPData$condition[grepl("RP",PALPData$Block)] <- "RP" 
  PALPData$condition[grepl("PR",PALPData$Block)] <- "PR" 
  PALPData$condition[grepl("PP",PALPData$Block)] <- "PP" 
  # Merge Trials to Om / Com Sum
  PALPDatasummary<-merge(plyr::count(PALPData[!grepl("summary", PALPData$Trial),], c("User.code","condition")),
                         aggregate(cbind(Om,Com)~User.code+condition, data=PALPData, sum, na.rm=T), by=c("User.code","condition"),all.x=T)
  # Add RT mean Overall
  PALPDatasummary<-merge(PALPDatasummary,
                         aggregate(Response.time..ms.~User.code+condition, data=PALPData[!grepl("summary", PALPData$Trial),], mean, na.rm=T), by=c("User.code","condition"),all.x=T)
  # Add RT mean for Commissions
  PALPDatasummary<-merge(PALPDatasummary,
                         aggregate(Response.time..ms.~User.code+condition, data=PALPData[PALPData$Com==1,], mean, na.rm=T), by=c("User.code","condition"),all.x=T)
  # Add RT mean for Correct Hits
  PALPDatasummary<-merge(PALPDatasummary,
                         aggregate(Response.time..ms.~User.code+condition, data=PALPData[PALPData$Response=="space",], mean, na.rm=T), by=c("User.code","condition"),all.x=T)
  # Add final score
  PALPDatasummary<-merge(PALPDatasummary,
        aggregate(as.numeric(Trial.result)~User.code+condition, data=PALPData[grep("summary", PALPData$Trial),], mean, na.rm=T), by=c("User.code","condition"),all.x=T)
  names(PALPDatasummary)<-c("User.code", "condition", "Trials", "Om", "Com", "RT_mean","RT_Com_mean","RT_Hit_mean", "Score")
  
  PALPDatasummary<-subset(PALPDatasummary, Trials %in% c(51,42,80))
  PALPDataQC<-subset(PALPDatasummary, select=c(User.code, condition))
  PALPDataQC$QC_PALP_TOOFAST <- ifelse(PALPDatasummary$RT_mean<200,yes = 1,no = 0)
  #In the rare cases where they made NO responses they should not have a missing for the QC FLAG as it raises a warning when aggregating
  PALPDataQC$QC_PALP_TOOFAST[is.na(PALPDatasummary$RT_mean)] <- 0
  
  # Create the worse than chance flag using defined as not significantly better than random performance at the 5% level
  # This creates a LOT of exclusions and perhaps should be revised a large number of Ppts seem to adopt a very weighted response strategy under some conditions
  PALPDataQC$QC_PALP_WORSE_THAN_CHANCE <- 0
  PALPDataQC$QC_PALP_WORSE_THAN_CHANCE[PALPDatasummary$Trials == 42 & (PALPDatasummary$Om >17|PALPDatasummary$Com >17)] <- 1
  PALPDataQC$QC_PALP_WORSE_THAN_CHANCE[PALPDatasummary$Trials == 80 & (PALPDatasummary$Om >30|PALPDatasummary$Com >30)] <- 1
  
  # Convert Om and Com to proportion (half trials could be om and half could be com) and remove the Trials variable
  PALPDatasummary$Om<-PALPDatasummary$Om / (PALPDatasummary$Trials /2)
  PALPDatasummary$Com<-PALPDatasummary$Com / (PALPDatasummary$Trials /2)
  PALPDatasummary<-subset(PALPDatasummary, select=-Trials)
  
  # Produce the Hit Correct NR columns, (1-Omission, 1-Comission)
  PALPDatasummary$Hits<-(1-PALPDatasummary$Om)
  PALPDatasummary$CorrectNR<-(1-PALPDatasummary$Com)
  
  # Calculate non-parametric SDT outcome variables
  aprime <-function(hit,fa) {
    a<-1/2+((hit-fa)*(1+hit-fa) /
              (4*hit*(1-fa)))
    b<-1/2-((fa-hit)*(1+fa-hit) /
              (4*fa*(1-hit)))
    a[fa>hit]<-b[fa>hit]
    a[fa==hit]<-.5
    a
  }
  bppd <-function(hit,fa) {
    ((1-hit)*(1-fa)-hit*fa) /
      ((1-hit)*(1-fa)+hit*fa)
  }
  PALPDatasummary$aprime<-aprime(PALPDatasummary$Hits,PALPDatasummary$Com)
  PALPDatasummary$bppd<-bppd(PALPDatasummary$Hits,PALPDatasummary$Com)
  
  #Convert to wide format - QC is aggregated so having a QC flag on one condition raises it for their whole attempt
  #Suppress warnings in the QC cast as it raises a pointless warning!
  PALPDataQC<-suppressWarnings(dcast(melt(PALPDataQC, id.vars=1:2), User.code ~ variable, fun.aggregate=max))
  PALPDatasummary<-dcast(melt(PALPDatasummary, id.vars=1:2), User.code ~ condition+variable)
  
  #merge QC and return
  PALPDatasummary<-merge(PALPDatasummary,PALPDataQC, by="User.code", all.x=T)
}

#' WCST Summary data
#' Generates the summary vriables for WCST task
#' @param WCSTData dataframe containing the long format WCST data
#' @keywords download dataset
summaryVarsWCST<-function(WCSTData){
  options( stringsAsFactors=F )
  WCSTData <- suppressWarnings(cbind(WCSTData, data.frame(do.call('rbind', strsplit(as.character(WCSTData$Trial.result),'_',fixed=TRUE)))))
  names(WCSTData)[names(WCSTData) == 'X2'] <- 'SortCategory'
  WCSTData$Perseverations[WCSTData$X3=='PERSEV']<-1
  WCSTData$Corrects[WCSTData$X1=='PASS']<-1
  WCSTData<-subset(WCSTData, select=-c(X1, X3))
  # Flag each switch for summing
  WCSTData$Switches<-0
  WCSTData$Switches[WCSTData$SortCategory != c(WCSTData$SortCategory[-1], NA) & WCSTData$User.code==c(WCSTData$User.code[-1], NA) & WCSTData$Iteration==c(WCSTData$Iteration[-1], NA) ]<-1
  
  #Summaries
  WCSTDataSums<-do.call(data.frame, aggregate(cbind(Corrects, Switches, Perseverations)~User.code, FUN=sum, na.rm=TRUE, na.action=NULL, data=WCSTData))
  WCSTDataSums<-merge(WCSTDataSums, do.call(data.frame, aggregate(cbind(Response.time..ms.)~User.code, function(x) 
    c(mean = mean(x), sd = sd(x)), data=WCSTData)), by=c("User.code"))
}

#' BART Summary data
#' Generates the summary vriables for BART task
#' @param BARTData dataframe containing the long format BART data
#' @keywords download dataset
summaryVarsBART<-function(BARTData){
  options( stringsAsFactors=F )
  BARTData <- cbind(BARTData, data.frame(do.call('rbind', strsplit(as.character(BARTData$Trial.result),'_',fixed=TRUE))))
  names(BARTData)[names(BARTData) == 'X1'] <- 'TrialResult'
  names(BARTData)[names(BARTData) == 'X2'] <- 'PumpsMade'
  BARTData$PumpsMade<-as.numeric(BARTData$PumpsMade)
  #Summaries
  BARTDataSums<-do.call(data.frame, aggregate(cbind(PumpsMade)~User.code+TrialResult, FUN=sum, na.rm=TRUE, na.action=NULL, data=BARTData))
  BARTDataSums<-reshape(BARTDataSums, direction = "wide", idvar = c( "User.code"), timevar = "TrialResult")
  BARTDataSums<-merge(do.call(data.frame, aggregate(cbind(TrialResult)~User.code, function(x)
    c(NumPopped = length(which(x == "POPPED"))), data=BARTData)), BARTDataSums, by=c("User.code"))
  names(BARTDataSums)[names(BARTDataSums) == 'TrialResult'] <- 'NumPopped'
  return(BARTDataSums)
}

