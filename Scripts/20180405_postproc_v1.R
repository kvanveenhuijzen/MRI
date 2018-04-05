library(data.table)
library(readxl)
library(plyr);library(dplyr)
library(tableone)
library(survival);library(ggplot2)
library(survminer)

##################
### LOAD DATA  ###
##################

# Data from ResearchR
cross_raw <- d4
long_raw <- long4


#########################
### ADD DATA TO CROSS ###
#########################

# Data from Genotypes.


################################
###  ADD STANDARD VARIABLES  ###
################################

# CROSS SECTIONAL
# Time related variables
dcross1 <- copy(cross_raw)
dcross1[, c("AoO", "AoDiag") := lapply(.SD, ageify, DoBirth=DoB), .SDcols=c("DoO","DoDiag")]
dcross1[, DiagDelay := ageify(DoDiag,DoO, format="months")]
dcross1[, DoSurv := DoDeath][is.na(DoDeath), DoSurv := DoCheck]  # Feitelijk een ifelse statement.
dcross1[, SurvOnset := ageify(DoSurv, DoO, format="months")]

# Factor variables
dcross1[, Dead :=  as.factor(as.numeric(!is.na(DoDeath))) ]
dcross1[, FTD := factor(ifelse(ALSplus=="FTD", "Yes", "No"))]


# LONGITUDINAL
# Time-ralated variables
dlong1 <- lapply(long_raw, function(i){
  DoEvent <- grep("Do", colnames(i), value = TRUE)
  AoEvent <- gsub("^D","A", DoEvent)
  out1 <- merge(i, cross_raw[,.(ALSnr,DoB), on="ALSnr"])
  out1[, (AoEvent) := lapply(.SD, ageify, DoBirth=DoB), .SDcols = DoEvent ]
  out1[,DoB:=NULL]
  return(out1)
})

# Get Onset2Event slopes
dlong1 <- lapply(dlong1, function(i){
  DoEvent <- grep("Do", colnames(i), value = TRUE)
  Event <-  gsub("^Do","",DoEvent)
  Onset2Event <- paste0("Onset2",Event)
  out1 <- merge(i, cross_raw[,.(ALSnr,DoO), on="ALSnr"])
  out1[, (Onset2Event) := lapply(.SD, ageify, DoBirth=DoO, format="months"), .SDcols = DoEvent ]
  out1[,DoO:=NULL]
  return(out1)
})
dlong1$ALSFRS_R[, ALSFRS_slope := (48-ALSFRS_score)/Onset2ALSFRS]  


#########################
###  WORKING OBJECTS  ###
#########################

firstScores1 <- lapply(dlong1, function(i){
  DoEvent <- grep("Do", colnames(i), value = TRUE)
  out1 <- i[, .SD[which.min(get(DoEvent))], by= ALSnr]
  return(out1)
}) 


###################
###  table one  ###
###################

#### VOORBEELD ####
ALS1 <- dcross1[Diag=="ALS"]
ALS1 <- merge(ALS1, firstScores1$ALSFRS_R, all.x = TRUE, by="ALSnr")

#
vars <- c("Gender", "AoO", "AoDiag", "SoO", "SurvOnset", "Familial", "FTD", "ALSFRS_score","ALSFRS_slope", "ISCED")
factorVars <- c("Gender", "SoO", "Familial", "FTD", "ISCED")
varsCont <- c("AoO", "AoDiag", "SoO", "SurvOnset", "ALSFRS_score","ALSFRS_slope")

tableOne <- CreateTableOne(vars = vars, strata = c("SoO"), data = ALS1, factorVars = factorVars, includeNA = T)
print(tableOne, nonnormal= varsCont, minMax=T, cramVars = "Gender", quote=T, noSpaces = T, missing = TRUE)

##################
###  Survival  ###
##################

surv1 <- survfit(Surv(SurvOnset, Dead==1) ~ SoO, ALS1)
ggsurvplot(surv1, conf.int = T, xlim=c(0,240))


