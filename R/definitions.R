# definitions

# COMMENT: variables in definitions must start with DEFINITION_

# ALSFRS-R, vraag 5
#  indien zowel vraag 5a als vraag 5b ingevuld is, kies ik er voor om vraag 5b te gebruiken
#  aangezien die alleen ingevuld kan worden als patient daadwerkelijk een PEG heeft.
#  Uiteraard ook registreren of je vraag 5a of 5b gebruikt hebt
# Let op! data wordt "in place" gemodificeerd. Wegschrijven naar een (nieuw) object is dus niet nodig.
DEFINITION_q5 <- function(A = "ALSFRS_q5a", B = "ALSFRS_q5b", data = long3$ALSFRS_R){
  if (!is.null(data)) {
    data[!is.na(get(A)), ALSFRS_q5:=get(A)]
    data[!is.na(get(B)), ALSFRS_q5:=get(B)]
    data[, ALSFRS_q5_PEG:=!is.na(get(B))]
    data[, c(A, B):=NULL]
  }
}

#namen van vragen van de ALSFRS-R
DEFINITION_Q_ALSFRSR <- function(rename = rename1){
  orign <- grep("Table ALS-FRS-R.[0-9][0-9]\\.|Table ALS-FRS-R.[0-9]\\.", rename$Original)
  ren <- c("ALSFRS_q5", rename1$Rename[orign]) #ALSFRS_q5 uit DEFINITION_q5
  return(ren)
}


#' Clean up ECAS behaviour fields
#' 
#' Door het vinkjessysteem in Progeny zullen veel velden voor ECAS behaviour subscores standaard 
#' op \code{N} staan. Deze functie zet alle velden op \code{NA} indien:
#' 1) Alle behaviour sumscores \code{NA} zijn EN 
#' 2) Alle subscores \code{NA} of \code{N} zijn.
#' 
#' @param data een data.table met longitudinale ECAS data. Default \code{long3$ECAS}
#' 
#' @return Dezelfde data.table met longitudinale ECAS data, waarbij foutieve velden op NA zijn 
#' gezet en rijen zonder ECAS data zijn verwijderd.
#' 
#' 
DEFINITION_ECAS_BHV <- function(data = long3$ECAS){
  if (is.null(data)){
    return(NULL)
  }
  ECASbhvfields <- c("ECAS_disinh", "ECAS_apath", "ECAS_sympathloss", "ECAS_persev", "ECAS_hyperoral")
  ECASbhvtot <- c("ECAS_bhvtotal","ECAS_bhvsymptom")
  
  
  criterion1 <- apply(is.na(data[, ECASbhvtot, with = FALSE]), 1, all) # Behaviour sumscores zijn beide NA
  criterion2 <- apply(data[, ECASbhvfields, with = FALSE]=="N" | is.na(data[, ECASbhvfields, with = FALSE]),
                      1, all) # Alle behaviour items moeten of NA, of N zijn.
  
  # Als dit beide het geval is, mogen de kolommen op NA worden gezet.
  set(data, i = which(criterion1 & criterion2), j = ECASbhvfields, value = NA)
  
  # Rijen die geen data hebben anders dan ALSnr en ProgenyFU, mogen weg.
  out1 <- data[rowSums(!is.na(data))>2,]
  return(out1)
}

