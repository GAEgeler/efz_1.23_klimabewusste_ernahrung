#load till data for popularity

#author: gian-andrea egeler
#state: february 2021
  

#load data
source("R/config_path.R")
source("R/03_read_selling_data_PR1.R", encoding = "UTF-8") 
source("R/02_read_selling_data_PR3.R", encoding = "UTF-8") #warnings can be ignored
source("R/01_read_selling_data_PR5.R", encoding = "ISO-8859-1")
source("R/04_read_selling_data_PR6.R") 


# bind all dataframes and do some mis en place for better search
df_total <- bind_rows(selling_PR1, selling_PR3, selling_PR5, selling_PR6)
df_total$meal_component <-  stringi::stri_trans_general(df_total$meal_component,"Latin-ASCII") #, # replace all umlaute
df_total$meal_component <-  stringr::str_replace(df_total$meal_component, '\\*', '') # repalce all asteriks
df_total$meal_component <- stringr::str_replace_all(df_total$meal_component, "[[:punct:]]", " ") # remove all puntuaktions https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

# save if file does not exist 
file_to_save = "all_sellings_pr_2019.csv"
readr::write_delim(df_total, here::here("augmented data", file_to_save), delim = ",")


#rm files
rm(file_to_save, selling_PR1, selling_PR3, selling_PR5, selling_PR6)

