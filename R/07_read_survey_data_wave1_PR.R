####### load survey data (first wave): this is a script generated from soscisurvey------
# state: November 2020


# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.
ds_file = here::here("raw data/survey/PR/rdata_mittagessen_1_2020-11-17_17-15.csv")



# read data
ds <- readr::read_delim(file = ds_file, 
                        delim = "\t", col_names = TRUE,
                        trim_ws = TRUE,
                        na = "-9",
                        locale = readr::locale(encoding = "latin1"))


# coalesce 2 complementary variables
ds_1 <- ds %>%
  #question concerning the city meal offer
  mutate(MO01_01 = coalesce(.$MO01_01, .$MO02_01)) %>% 
  mutate(MO01_02 = coalesce(.$MO01_01, .$MO02_02)) %>%
  mutate(MO01_03 = coalesce(.$MO01_01, .$MO02_03)) %>%
  mutate(MO01_04 = coalesce(.$MO01_01, .$MO02_04)) %>%
  #question concerning the food features
  mutate(FF01_01 = coalesce(.$FF01_01, .$FF02_01)) %>% 
  mutate(FF01_02 = coalesce(.$FF01_02, .$FF02_02)) %>% 
  mutate(FF01_03 = coalesce(.$FF01_03, .$FF02_03)) %>% 
  mutate(FF01_04 = coalesce(.$FF01_04, .$FF02_04)) %>% 
  mutate(FF01_05 = coalesce(.$FF01_05, .$FF02_05)) %>% 
  mutate(FF01_06 = coalesce(.$FF01_06, .$FF02_06)) %>% 
  mutate(FF01_07 = coalesce(.$FF01_07, .$FF02_07)) %>% 
  mutate(FF01_08 = coalesce(.$FF01_08, .$FF02_08)) %>%
  # question about the money preference
  mutate(MF01_01 = coalesce(.$MF01_01, .$MF02_01)) %>% 
  mutate(MF01_02 = coalesce(.$MF01_02, .$MF02_02)) %>% 
  mutate(MF01_03 = coalesce(.$MF01_03, .$MF02_03)) %>% 
  mutate(MF01_04 = coalesce(.$MF01_04, .$MF02_04))

# remove files
rm(ds, ds_file)