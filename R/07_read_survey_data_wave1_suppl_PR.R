####### load survey data (first wave, supplementary data): this is a script generated from soscisurvey-----
# state: November 2020


# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

ds_file = here::here("raw data/survey/PR/rdata_mittagessen_nacherfassung_2020-11-17_17-14.csv")
# setwd("./")
# ds_file = "rdata_mittagessen_nacherfassung_2020-11-17_17-14.csv"

# read data
ds <- readr::read_delim(file = ds_file, 
                        delim = "\t", col_names = TRUE,
                        trim_ws = TRUE,
                        na = "-9",
                        locale = readr::locale(encoding = "latin1"))


# remove files
ds_1_suppl <- ds
rm(ds, ds_file)
