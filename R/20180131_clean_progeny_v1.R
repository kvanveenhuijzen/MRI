# clean data Progeny
# version 0.1
# date: 31 January 2018
# test

##################
## ARCHITECTURE ##
##################

# to complete #############
# 1. rename
# 2. reclassify
# 3. check minimum
# 4. check maximum
# 5. check possible values
# 6. check regex
# 7. cross vs long data

##################
## INSTELLINGEN ##
##################

# package dir
DIR1 <- "/Volumes/Samsung_T1/Vakantie/HJ/Imaging/R_packages/MRI"

# modern Mac (nodig om dates in Excel file goed te lezen)
# een niet moderne Mac is gedefinieerd van voor 2011
# je kunt het in Excel controleren door in te typen: =DATEVALUE("1/1/2016")
# als hier 42370 uit komt is het een nieuwe Mac, als er 40908 uit komt is het een oude Mac
modern_Mac <- TRUE


###############
## FUNCTIONS ##
###############

# zelfde als setnames, maar dan voor classes van kolommen
# dt = data.table
# cols = vector met kolomnamen of kolomnummers die je wilt aanpassen
# new_class = vector met classes
setclass2 <- function(dt, cols, new_class){
  if (!is.data.table(dt)) 
    stop("dt is not a data.table")
  if (!length(attr(dt, "names"))) 
    stop("dt has no column names")
  if (length(cols) != length(new_class)) 
    stop("cols is length ", length(cols), " but new_class are length ", 
         length(new_class))
  
  dt0 <- copy(dt)
  dt <- dt[, ..cols]
  dt[] <- lapply(dt, function(x) as.character(x)) #change all classes to character
  for (i in unique(new_class)){
    coln1 <- which(new_class == i)
    class1 <- paste0("as.", i)
    dt[, (coln1) := lapply(.SD, class1), .SDcols = coln1][]
  }
  #merge weer met kolommen die niet in cols voorkwamen
  if(ncol(dt0)>0){
    dt <- cbind(dt, dt0[, which(!colnames(dt0) %in% colnames(dt)), with = FALSE])
  }
  return(dt)
}

# function for count changes
# values for cols:
## NULL             geeft 1 getal terug (over hele vector of dataframe)
## "all"            geeft data.table van 1 rij terug met alle variabelen
## c(colnumbers)    geeft data.table van 1 rij terug met alle gespecificeerde kolomnummers
## c(colnames)      geeft data.table van 1 rij terug met alle gespecificeerde kolomnamen
countNA <- function(data, cols=NULL){
  countNA_internal <- function(x) length(which(is.na(x)))
  
  if(is.null(cols)){
    orig_Nrow <- length(c(t(data)))
    data.table(countNA_internal(data))[,orig_Nrow:=orig_Nrow][]
  }else if(cols[1] == "all"){
    data[, lapply(.SD, function(x) countNA_internal(x))][,orig_Nrow:=nrow(data)][]
  }else{
    data[, lapply(.SD, function(x) countNA_internal(x)), .SDcols = cols][,orig_Nrow:=nrow(data)][]
  }
}
# countNA(bla1, cols = c("T", "W", "disp")) #voorbeeld

# functie om summary over NA's te geven
summaryNA <- function(old, new, count_var = "orig_Nrow", name_data = "data", reason = "..."){
  df1 <- rbind(old, new)
  Nrow <- df1[,get("orig_Nrow")]
  df1[,orig_Nrow:=NULL]
  
  if(length(unique(Nrow))==1){
    df2 <- df1[2,] - df1[1,]
    if(all(unlist(df2)==0)){
      no_change <- paste0("Data = ", name_data, "; applied function(s)/modification(s) didn't change the number of NA's.")
      return(no_change)
    }
    df3 <- roundHJ1(df2 / Nrow * 100, digits = 2)
    out1 <- list(variable = colnames(df2), increase_NA = unlist(df2), N = rep(Nrow[1], length(df3)), perc = df3)
    out2 <- setDT(out1)
    
    #gooi alle kolommen met increase_NA==0 weg
    out3 <- out2[increase_NA!=0]
    out3$Omschrijving <- paste0("Data = ", name_data, ", variable = ", out3$variable, "; ", out3$increase_NA, "/",
                                out3$N, " waardes (", out3$perc, "%)", " op NA gezet omdat ", reason)
    return(out3)
  }else{
    stop("Dit nog niet gescript.")
  }
}

#voor non-p-values (functie voor afronden)
roundHJ1 <- function(x, digits=1){
  out1 <- sprintf(paste0("%.", digits, "f"), round(x, digits = digits))
  return(out1)
}

# functie om makkelijker direct uit summaryNA toe te voegen aan mm (ook overzichten in bijv. data.table format)
# good alleen gebruiken als x een vector is
add_mm <- function(x, melding = NULL){
  if(is.null(dim(x))){
    if(is.null(melding)){
      x
    }else{
      paste0(melding, x)
    }
  }else{
    if(is.null(melding)){
      x
    }else{
      list(list(melding), list(x)) #nested list
    }
  }
}

#functie om functies te herkennen in format1
recog_func <- function(x){
  pos1 <- grep("^f\\(.*\\)$", x)
  func1 <- gsub("^f\\(|\\)$", "", x[pos1])
  #func1 <- gsub("^f\\(|\\)$", "", x)
  return(func1)
}
#recog_func(rename1$Maximum)

#functie om minimum en maximum te contoleren
minmax <- function(x, date = FALSE){
  if(length(x)>1){
    stop("x mag momenteel slechts 1 value bevatten")
  }
  if(is.na(x)){
    return(NA)
  }
  
  out1 <- recog_func(x)
  
  if(length(out1)==0){
    out1 <- as.numeric(x)
    if(date == TRUE){
      out1 <- excel_numeric_to_date(out1)
    }
  }else{
    out1 <- eval(parse(text = out1))
  }
  
  return(out1)
}
#minmax(rename1_date$Maximum[1])

#geleend van janitor package, maar geen zin om het hele package te installeren en te laden, dus zo gedaan
excel_numeric_to_date <- function (date_num, date_system = ifelse(modern_Mac == TRUE, "modern", "mac pre-2011")) 
{
  if (!is.numeric(date_num)) {
    stop("argument `date_num` must be of class numeric")
  }
  if (date_system == "mac pre-2011") {
    as.Date(date_num, origin = "1904-01-01")
  }
  else if (date_system == "modern") {
    as.Date(date_num, origin = "1899-12-30")
  }
  else {
    stop("argument 'created' must be one of 'mac pre-2011' or 'modern'")
  }
}


##########################
## LOAD DATA & PACKAGES ##
##########################

# load packages
library(readxl)
library(data.table)

# load data
d1 <- data.table(read_excel(path = paste0(DIR1, "/Progeny/20180130_progeny_v1.xlsx")))
format1 <- data.table(read_excel(path = paste0(DIR1, "/Data/Format_v1.xlsx")))


####################
## PREFORMAT DATA ##
####################

#maak copy van d1
d2 <- copy(d1)

#detele in format1 alle rijen waarin 1 # staat
format2 <- format1[ifelse(rowSums(format1 == "#", na.rm = TRUE)>0, FALSE, TRUE),]

#maak meldingen file
mm <- list("MELDINGEN")


####################
## DATA WRANGLING ##
####################

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

#check regex ################ pas later doen



