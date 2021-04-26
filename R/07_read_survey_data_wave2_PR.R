####### load survey data (second wave): this is a script generated from soscisurvey-----
# state: November 2020

# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

ds_file = here::here("raw data/survey/PR/rdata_mittagessen_2_2020-11-17_17-14.csv")


# read data
ds <- readr::read_delim(file = ds_file, 
                       delim = "\t", col_names = TRUE,
                       trim_ws = TRUE,
                       na = "-9",
                       locale = readr::locale(encoding = "latin1"))
  
# some of the same information are saved into different variables: 
# due to problems with PC and Mobile version
# see: https://stackoverflow.com/questions/55893938/merging-complementary-columns-in-r

# first change some data structure: there is a plus in a variable containg only
# numerical information
ds <- ds %>% 
  mutate(M302_01 = str_replace(M302_01, "[+]", "")) %>% 
  mutate(M302_01 = as.numeric(M302_01)) %>% 
  mutate(M302_02 = as.numeric(M302_02)) %>%
  mutate(M302_03 = as.numeric(M302_03)) %>%
  mutate(M302_04 = as.numeric(M302_04))


ds_2 <- ds %>%
  #question concerning info about menu (plan)
  mutate(M104 = coalesce(.$M104, .$M107)) %>% 
  mutate(M104_01 = coalesce(.$M104_01, .$M107_01)) %>% 
  mutate(M104_02 = coalesce(M104_02, M107_02)) %>%
  mutate(M104_03 = coalesce(M104_03, M107_03)) %>% 
  mutate(M104_04 = coalesce(M104_04, M107_04)) %>% 
  mutate(M104_05 = coalesce(M104_05, M107_05)) %>% 
  mutate(M104_06 = coalesce(M104_06, M107_06)) %>% 
  mutate(M104_07 = coalesce(M104_07, M107_07)) %>%
  mutate(M104_08 = coalesce(M104_08, M107_08)) %>%
  mutate(M104_09 = coalesce(M104_09, M107_09)) %>%
  mutate(M104_10 = coalesce(M104_10, M107_10)) %>%
  mutate(M104_10a = coalesce(M104_10a, M107_10a)) %>%
  # question why chooseing the meal
  mutate(M202 = coalesce(M202, M206)) %>%
  mutate(M202_01 = coalesce(M202_01, M206_01)) %>%
  mutate(M202_02 = coalesce(M202_02, M206_02)) %>%
  mutate(M202_03 = coalesce(M202_03, M206_03)) %>%
  mutate(M202_04 = coalesce(M202_04, M206_04)) %>%
  mutate(M202_05 = coalesce(M202_05, M206_05)) %>%
  mutate(M202_06 = coalesce(M202_06, M206_06)) %>%
  mutate(M202_07 = coalesce(M202_07, M206_07)) %>%
  mutate(M202_08 = coalesce(M202_08, M206_08)) %>%
  mutate(M202_09 = coalesce(M202_09, M206_09)) %>%
  mutate(M202_10 = coalesce(M202_10, M206_10)) %>%
  mutate(M202_11 = coalesce(M202_11, M206_11)) %>%
  mutate(M202_12 = coalesce(M202_12, M206_12)) %>%
  mutate(M202_13 = coalesce(M202_13, M206_13)) %>%
  mutate(M202_15 = coalesce(M202_15, M206_15)) %>%
  mutate(M202_16 = coalesce(M202_16, M206_16)) %>%
  mutate(M202_17 = coalesce(M202_17, M206_17)) %>%
  mutate(M202_17a = coalesce(M202_17a, M206_17a)) %>%
  # question attitude to the choice
  mutate(M203_01 = coalesce(M203_01, M204_01)) %>%
  mutate(M203_02 = coalesce(M203_02, M204_02)) %>%
  mutate(M203_03 = coalesce(M203_03, M204_03)) %>%
  mutate(M203_04 = coalesce(M203_04, M204_04)) %>%
  mutate(M203_05 = coalesce(M203_05, M204_05)) %>%
  mutate(M203_06 = coalesce(M203_06, M204_06)) %>%
  mutate(M203_07 = coalesce(M203_07, M204_07)) %>%
  mutate(M203_08 = coalesce(M203_08, M204_08)) %>%
  mutate(M203_09 = coalesce(M203_09, M204_09)) %>%
  mutate(M203_10 = coalesce(M203_10, M204_10)) %>%
  mutate(M203_11 = coalesce(M203_11, M204_11)) %>%
  mutate(M203_12 = coalesce(M203_12, M204_12)) %>%
  # question to city offer
  mutate(S101_01 = coalesce(S101_01, S102_01)) %>%
  mutate(S101_02 = coalesce(S101_02, S102_02)) %>%
  mutate(S101_03 = coalesce(S101_03, S102_03)) %>%
  mutate(S101_04 = coalesce(S101_04, S102_04)) %>%
  #question to food features
  mutate(F101_01 = coalesce(F101_01, F102_01)) %>%
  mutate(F101_02 = coalesce(F101_02, F102_02)) %>%
  mutate(F101_03 = coalesce(F101_03, F102_03)) %>%
  mutate(F101_04 = coalesce(F101_04, F102_04)) %>%
  mutate(F101_05 = coalesce(F101_05, F102_05)) %>%
  mutate(F101_06 = coalesce(F101_06, F102_06)) %>%
  mutate(F101_07 = coalesce(F101_07, F102_07)) %>%
  mutate(F101_08 = coalesce(F101_08, F102_08)) %>%
  # question concerning paying for sustainability
  mutate(M301_01 = coalesce(M301_01, M302_01)) %>%
  mutate(M301_02 = coalesce(M301_02, M302_02)) %>%
  mutate(M301_03 = coalesce(M301_03, M302_03)) %>%
  mutate(M301_04 = coalesce(M301_04, M302_04)) %>%
  # question to eat habit
  mutate(F201 = coalesce(F201, F202)) %>%
  mutate(F201_05 = coalesce(F201_05, F202_05)) %>%
  # question concerning UBP
  mutate(B008 = coalesce(B008, B016)) %>%
  # question concerning the intervantion change
  mutate(B004_01 = coalesce(B004_01, B011_01)) %>%
  mutate(B004_02 = coalesce(B004_02, B011_02)) %>%
  mutate(B004_03 = coalesce(B004_03, B011_03)) %>%
  mutate(B004_04 = coalesce(B004_04, B011_04)) %>%
  mutate(B004_05 = coalesce(B004_05, B011_05)) %>%
  # question concerning intervention change: ubp
  mutate(B009_01 = coalesce(B009_01, B013_01)) %>%
  mutate(B009_02 = coalesce(B009_02, B013_02)) %>%
  mutate(B009_03 = coalesce(B009_03, B013_03)) %>%
  mutate(B009_04 = coalesce(B009_04, B013_04)) %>%
  mutate(B009_05 = coalesce(B009_05, B013_05)) %>%
  # question intervention behavior pre post change
  mutate(B020_01 = coalesce(B020_01, B021_01)) # attention scale is different

#remove some files
rm(ds, ds_file)

