# clean data Progeny
# version 0.1
# date: 31 January 2018

# TODO:
# 1. testhat inbouwen

######################
##  ARCHITECTURE  ####
######################

# to complete #############-
# 1. rename
# 2. reclassify
# 3. check minimum
# 4. check maximum
# 5. check possible values
# 6. check regex
# 7. cross vs long data


####################
####  SETTINGS  ####
####################

# package dir
DIR1 <- "/Volumes/Samsung_T1/Vakantie/HJ/Imaging/R_packages/MRI"

# source settings
source(paste0(DIR1, "/R/settings.R"))


#####################
####  FUNCTIONS  ####
#####################

# source functions
source(paste0(DIR1, "/R/functions.R"))


################################
####  LOAD DATA & PACKAGES  ####
################################

# load packages
library(readxl)
library(data.table)

# load data
d1 <- data.table(read_excel(path = paste0(DIR1, "/Progeny/20180130_progeny_v1.xlsx")))
format1 <- data.table(read_excel(path = paste0(DIR1, "/Data/Format_v1.xlsx")))


##########################
####  PREFORMAT DATA  ####
##########################

# maak copy van d1
d2 <- copy(d1)

# detele in format1 alle rijen waarin 1 # staat
format2 <- format1[ifelse(rowSums(format1 == "#", na.rm = TRUE)>0, FALSE, TRUE),]

# maak meldingen file
mm <- list("MELDINGEN")


##########################
####  DATA WRANGLING  ####
##########################

##########################
####  RENAME COLUMNS  ####
##########################

#check whether all columnnames in d1 exist in format2.
not_in_format2 <- colnames(d1)[!colnames(d1) %in% format2$Original]
if(length(not_in_format2)>0){
  mm <- c(mm, paste0("MOGELIJKE FOUT: ", paste(not_in_format2, collapse = ", "),
                     if(length(not_in_format2)>1) " komen" else " komt",
                     " niet in format2 voor, maar wel in d1. Deze variabelen hebben hun originele naam gehouden."))
  
}else{
  mm <- c(mm, "GOED: Alle variabelen in d1 komen ook voor in d1.")
}

#rename colnames
rename1 <- merge(data.table(Original = colnames(d2), pos = 1:ncol(d2)), format2, by = "Original", all.x = TRUE)
setkey(rename1, pos)
setnames(d2, old = rename1[!is.na(Rename)]$Original, new = rename1[!is.na(Rename)]$Rename)
mm <- c(mm, "GOED: kolomnamen in d2 aangepast (conform format2).")


#######################
####  ADJUST CLASS ####
#######################

#adjust class
old1 <- countNA(d2, cols = "all")
d2 <- setclass2(d2, cols = rename1[!is.na(Class)]$Rename, new_class = rename1[!is.na(Class)]$Class)
new1 <- countNA(d2, cols = "all")

#melding over adjust class in mm
summary1 <- summaryNA(old = old1, new = new1, count_var = "orig_Nrow", name_data = "d2",
                      reason = "de opgelegde dataclass niet overeenkwam met de werkelijke class (=FOUT)")
melding1 <- ifelse(is.vector(summary1), "GOED: ",
                   "FOUT: hieronder een overzicht met variabelen waarvan het aantal NA's veranderde door de class aan te passen (Data = d2)")
mm <- c(mm, add_mm(x = summary1, melding = melding1))


#####################################
####  CHECK MINIMUM AND MAXIMUM  ####
#####################################

#split rename1 in date en nondate
rename1_date1 <- rename1[Class=="Date"]
rename1_nondate1 <- rename1[Class!="Date"]

#controleer minimum en maximum voor non-dates
old1 <- countNA(d2, cols = "all")
for (x in 1:nrow(rename1_nondate1)){
  min1 <- minmax(rename1_nondate1$Minimum[x])
  max1 <- minmax(rename1_nondate1$Maximum[x])
  if(!is.na(min1)){
    set(d2, i = which(d2[[rename1_nondate1$Rename[x]]] < min1), j = rename1_nondate1$Rename[x], value = NA)
  }
  if(!is.na(max1)){
    set(d2, i = which(d2[[rename1_nondate1$Rename[x]]] > max1), j = rename1_nondate1$Rename[x], value = NA)
  }
}
new1 <- countNA(d2, cols = "all")
reason1 <- "de waarde van deze variabele(n) niet tussen de gedefinieerde minimale en maximale waarden lag."
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))

#controleer minimum en maximum voor dates
old1 <- countNA(d2, cols = "all")
for (x in 1:nrow(rename1_date1)){
  min1 <- minmax(rename1_date1$Minimum[x], date = TRUE)
  max1 <- minmax(rename1_date1$Maximum[x], date = TRUE)
  if(!is.na(min1)){
    set(d2, i = which(d2[[rename1_date1$Rename[x]]] < min1), j = rename1_date1$Rename[x], value = NA)
  }
  if(!is.na(max1)){
    set(d2, i = which(d2[[rename1_date1$Rename[x]]] > max1), j = rename1_date1$Rename[x], value = NA)
  }
}
new1 <- countNA(d2, cols = "all")
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))


#################################
####  CHECK POSSIBLE VALUES  ####
#################################

# check possible values
val1 <- rename1[!is.na(Possible_values)]
val2 <- strsplit(val1$Possible_values, split = "\\|")
old1 <- countNA(d2, cols = "all")
for (x in 1:length(val2)){
  set(d2, i = which(!d2[[val1$Rename[x]]] %in% val2[[x]]), j = val1$Rename[x], value = NA)
}
new1 <- countNA(d2, cols = "all")
reason1 <- "de waarde van deze variabele(n) niet voorkwam in de 'possible values'."
mm <- c(mm, list(summaryNA(old1, new1, name_data = "d2", reason = reason1)))


#######################
####  CHECK REGEX  ####
#######################

#check regex ################ pas later doen



