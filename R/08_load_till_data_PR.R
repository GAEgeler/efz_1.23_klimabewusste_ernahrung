#load till data of PR

#state: january 2021
#author: gian-andrea egeler

#required functions
source("R/08_function_mis_en_place_PR.R")
source("R/08_function_read_selling_data_PR.R")

#load paths
source("R/config_path.R")

############
##2019
###########

#load data pr4------

#set path
path = paste0(till_pr, "PR4/2019")

#lookup for single files
list_files <- list.files(path, pattern = ".csv", full.names = TRUE)

dt_PR4 <- list_files %>% 
  purrr::map_dfr(~ read_csv_1_custom_pr(., n = 10, var_date = "x7")) %>%  # there are some  warnigns (2), which can be ignored

  #2) with the variable names x2, x3, ...
  mutate(pr = "PR4")
#check if all csv where read
message("ℹ ", dplyr::n_distinct(dt_PR4$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#-----------------------------------------------------------------------------------


#load data PR3------

#set path
path = paste0(till_pr, "PR3/2019")

#lookup for single files
list_files <- list.files(path, pattern = ".xlsx", full.names = TRUE)

dt_PR3 <- list_files %>% 
  purrr::map_df(~ read_xlsx_1_custom_pr(., n = 12)) %>%  # there are 2 kinds of errors, both can be ignored
#1) with the date because of the timezone and
#2) with the variable names x2, x3, ...
  dplyr::mutate(pr = "PR3")
  
#check if all csv where read
message("ℹ ", dplyr::n_distinct(dt_PR3$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#-----------------------------------------------------------------------------------

#load data PR2-------

#set path
path = paste0(till_pr, "PR2/2019/")


list_files <- list.files(path, pattern = ".csv", full.names = TRUE)

dt_PR2 <- list_files %>% 
  purrr::map_dfr(~ read_csv_1_custom_pr(., n = 10, var_date = "x7")) %>%  # there are some of errors, which can be ignored
  #2) with the variable names x2, x3, ...
  dplyr::mutate(pr = "PR2") %>% 
  #there are some weird transactions with thousand separator
  #thus the most of the variables were characters
  dplyr::mutate(tot_sold = as.numeric(.$tot_sold),
         avg_price = as.numeric(.$avg_price),
         total_amount = as.numeric(str_replace(.$total_amount, ",", "")))


#check if all csv where read
message("ℹ ", dplyr::n_distinct(dt_PR2$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")




#-----------------------------------------------------------------------------------

#load data PR1 (not from the tcpos)

#define path with file
file = paste0(till_pr, "PR1/2019/Artikelanalyse_august_november_2019_egel.xlsx")

#put all sheets into a list
dt_PR1 <- file %>%
  readxl::excel_sheets() %>% 
  purrr::set_names() %>%
  # concatenate columnwise
  purrr::map_dfc(~ readxl::read_xlsx(., path = file, range = "A4:AF24")) %>% 
  #drop columns, we do not care
  dplyr::select(97:223) %>% 
  dplyr::select(-...160, -Datum...193, -Datum...161, -Datum...129) %>% #drop variables containing datum
  tidyr::pivot_longer(-(Datum...97), names_to = "date", values_to = "tot_sold") %>% 
  dplyr::mutate(date_ = lubridate::as_date(as.numeric(.$date), origin = "1899-12-30")) %>% # extract date https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r 
  tidyr::drop_na(tot_sold) %>% # drop na's in tot_sold
  dplyr::select(date_,  Datum...97, tot_sold) %>% 
  dplyr::rename(date = date_, article_description = Datum...97) %>% 
  dplyr::mutate(pr = "PR1")



#####
# clean up data
#####

dtPR3 <- mis_en_place_pr3(dt_PR3)
dtPR4 <- mis_en_place_pr4(dt_PR4)
dtPR2 <- mis_en_place_pr2(dt_PR2)
dtPR1 <- mis_en_place_pr1_1(dt_PR1)

###
# paste together data
###

dt_pr_2019 <- dplyr::bind_rows(dtPR1, dtPR2, dtPR3, dtPR4)

###
# add meal_label
###
dt_pr_2019 <- add_meal_label_pr(dt_pr_2019)

###
##save
###
readr::write_delim(dt_pr_2019, here::here("augmented data/sellings_pr_2019.csv"), delim = ",")

############
##2020
###########

#load data PR4------

#set path
path = paste0(till_pr, "PR4/2020")

#lookup for single files
list_files <- list.files(path, pattern = ".csv", full.names = TRUE)

dt_PR4 <- list_files %>% 
  purrr::map_dfr(~ read_csv_1_custom_pr(., n = 10, var_date = "x7")) %>%  # there are some of errors, which can be ignored
  #1) with the variable names x2, x3, ...
  dplyr::mutate(pr = "PR4")

#check if all csv where read
message("ℹ ",dplyr::n_distinct(dt_PR4$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#-----------------------------------------------------------------------------------

#load data PR3------

#set path
path = paste0(till_pr, "PR3/2020")

#lookup for single files
list_files <- list.files(path, pattern = ".xlsx", full.names = TRUE)

dt_PR3 <- list_files %>% 
  purrr::map_df(~ read_xlsx_2_custom_pr(., n = 20, var_date = "x10")) %>%  # there are some errors,  can be ignored
  #2) with the variable names x2, x3, ...
  dplyr::mutate(tot_sold = as.numeric(.$tot_sold),
         avg_price = as.numeric(.$avg_price),
         total_amount = as.numeric(.$total_amount)) %>% 
  dplyr::mutate(pr = "PR3")

#check if all csv where read
message("ℹ ",dplyr::n_distinct(dt_PR3$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#-----------------------------------------------------------------------------------


#load data PR2-------

#set path
path = paste0(till_pr, "PR2/2020/")


list_files <- list.files(path, pattern = ".csv", full.names = TRUE)

dt_PR2 <- list_files %>% 
  purrr::map_dfr(~ read_csv_2_custom_pr(., n = 11, var_date = "x8")) %>%  # there are some errors, which can be ignored
  #there are some weird transactions with thousand separator
  #thus the most of the variables were characters
  dplyr::mutate(tot_sold = as.numeric(.$tot_sold),
         avg_price = as.numeric(.$avg_price),
         total_amount = as.numeric(str_replace(.$total_amount, ",", ""))) %>% 
  dplyr::mutate(pr = "PR2")

#check if all csv where read
message("ℹ ", dplyr::n_distinct(dt_PR2$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#-----------------------------------------------------------------------------------

#laod data PR1

#define path
path = paste0(till_pr, "PR1/2020")

#lookup for single files
list_files <- list.files(path, pattern = ".csv", full.names = TRUE)

dt_PR1 <- list_files %>% 
  purrr::map_df(~ read_csv_1_custom_pr(., n = 10, var_date = "x7")) %>%  # there are some errors,  can be ignored
  #1) with the variable names x2, x3, ...
  #2) parsing failures at the beginning => due to weird delimiter in the csv file "01-09-20 Raport.csv"
  #some wird transactions are still there => mis_en_place takes care of that
  dplyr::mutate(tot_sold = as.numeric(.$tot_sold),
         avg_price = as.numeric(.$avg_price),
         total_amount = as.numeric(.$total_amount)) %>% 
  dplyr::mutate(pr = "PR1")

#check if all csv where read
message("ℹ ", dplyr::n_distinct(dt_PR1$date), " dates are in the dataset.", "\n", "ℹ ", length(list_files), " is the lenght of the csv files")


#####
# clean up data
#####

dtPR3 <- mis_en_place_pr3(dt_PR3)
dtPR4 <- mis_en_place_pr4(dt_PR4) 
dtPR2 <- mis_en_place_pr2(dt_PR2) 
dtPR1 <- mis_en_place_pr1_2(dt_PR1)


###
# add info about intervention pr2
###

#read info about intervention and meal_content
offer_pr2 <- readr::read_delim(paste0(till_pr, "PR2/intervention_pr2.csv"), 
                               delim = ";",
                               #check for the right encoding
                               locale = readr::locale(encoding = "iso-8859-1")) %>% 
  mutate(date = lubridate::as_date(date, format = "%d.%m.%Y"))

meal_content_pr2 <- readr::read_delim(paste0(till_pr, "PR2/identical_meal_line_pr2.csv"),
                                        delim = ";", 
                                        #check for the right encoding
                                        locale = readr::locale(encoding = "iso-8859-1")) %>% 
  dplyr::mutate(date = lubridate::as_date(date, format = "%d.%m.%Y"))


#1) filter out only the intervention time, attention the meal content is here (58 observations)
intervention_pr2 <- dtPR2 %>% 
  dplyr::inner_join(., offer_pr2)  # 2020-09-14 is missing because of a holiday


#2) filter out only the weeks before the intervention kw35,36 (10 observations)
identical_pr2 <- dplyr::anti_join(dtPR2, intervention_pr2) %>% 
   dplyr::inner_join(., meal_content_pr2)

#3) filter out the rest and add meal content
rest_pr2 <- dplyr::bind_rows(identical_pr2, intervention_pr2) %>% 
  dplyr::anti_join(dtPR2, .) %>%
  add_meal_label_pr(.)

#4) bind all together
dsPR2 <- bind_rows(intervention_pr2, identical_pr2, rest_pr2)

###
# paste together the rest of the three pr's and add meal_content
###
meal_content <- readr::read_delim(paste0(till_pr, "identical_meal_line_pr.csv"),
                                        delim = ";",
                                  locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::mutate(date = lubridate::as_date(date, format = "%d.%m.%Y"))


#paste them together and add meal_label
dt_pr_ <- dplyr::bind_rows(dtPR1, dtPR3, dtPR4) %>% 
  dplyr::left_join(., meal_content, by = c("date", "meal_line"))


#add the rest of the meal label
dt_pr_20 <- dt_pr_ %>% 
  #drop all meals containing information, so that nothing is overwritten
  dplyr::filter(is.na(meal_content)) %>% 
  add_meal_label_pr(.)
  
#bind the rest together
dt_pr_2020 <- dt_pr_ %>% 
  dplyr::filter(!is.na(meal_content)) %>% 
  dplyr::bind_rows(., dt_pr_20)

#bind all together, with the information of the pr2
dt_pr_2020 <- dplyr::bind_rows(dsPR2, dt_pr_2020)


###
##save
###
readr::write_delim(dt_pr_2020, here::here("augmented data/sellings_pr_2020.csv"), delim = ",")


#remove
rm(list = c("before_after_intervention", "dsPR2", "dt_pr_", "dt_pr_20", "dt_PR1", "dt_PR2", 
   "dt_PR3", "dt_PR4", "dtPR1", "dtPR2", "dtPR3", "dtPR4", "list_files", "offer_pr2", "path", "meal_content"))
