# definitions

# COMMENT: variables in definitions must start with DEFINITION_

# ALSFRS-R, vraag 5
#  indien zowel vraag 5a als vraag 5b ingevuld is, kies ik er voor om vraag 5b te gebruiken
#  aangezien die alleen ingevuld kan worden als patient daadwerkelijk een PEG heeft.
#  Uiteraard ook registreren of je vraag 5a of 5b gebruikt hebt
# Let op! data wordt "in place" gemodificeerd. Wegschrijven naar een (nieuw) object is dus niet nodig.
DEFINITION_q5 <- function(A = "ALSFRS_q5a", B = "ALSFRS_q5b", data = long3$ALSFRS_R){
  data[!is.na(get(A)), ALSFRS_q5:=get(A)]
  data[!is.na(get(B)), ALSFRS_q5:=get(B)]
  data[, ALSFRS_q5_PEG:=!is.na(get(B))]
  data[, c(A, B):=NULL]
}

#namen van vragen van de ALSFRS-R
DEFINITION_Q_ALSFRSR <- function(rename = rename1){
  orign <- grep("Table ALS-FRS-R.[0-9][0-9]\\.|Table ALS-FRS-R.[0-9]\\.", rename$Original)
  ren <- c("ALSFRS_q5", rename1$Rename[orign]) #ALSFRS_q5 uit DEFINITION_q5
  return(ren)
}

