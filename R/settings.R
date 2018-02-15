# settings

# COMMENT: variables in settings must be spelled with only CAPITALS

# DIR1
# DIR1 is de enige setting die in 20180131_clean_progeny_v1.R gedefinieerd wordt

# modern Mac (nodig om dates in Excel file goed te lezen)
# een niet moderne Mac is gedefinieerd van voor 2011
# je kunt het in Excel controleren door in te typen: =DATEVALUE("1/1/2016")
# als hier 42370 uit komt is het een nieuwe Mac, als er 40908 uit komt is het een oude Mac
MODERN_MAC <- TRUE
