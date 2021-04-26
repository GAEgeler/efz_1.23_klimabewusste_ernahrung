# read survey data (PR) --------

# status: february 2021
# author: gian-andrea egeler

#load functions
source("R/07_function_edit_survey_data_PR.R", encoding = "latin1") #returns a list

#load data wave 
source("R/07_read_survey_data_wave1_PR.R")
source("R/07_read_survey_data_wave1_suppl_PR.R")
source("R/07_read_survey_data_wave2_PR.R")

#wave 1
df_wave1 <- dplyr::bind_rows(ds_1, ds_1_suppl)

#edit and put into long format data of wave1
df_l <- edit_survey_pr(df_wave1, wave = "first")

#split list into two datasets
df_w1 <- df_l[[1]]
df_w1_wide <- df_l[[2]]


#edit data of wave2
df_l <- edit_survey_pr(ds_2, wave = "second")

df_w2 <- df_l[[1]]
df_w2_wide <- df_l[[2]]


# merge all data together
df_survey_pr <- dplyr::bind_rows(df_w1, df_w2)

#remove files
rm(df_wave1, df_w1, df_l, df_w2, ds_1, ds_1_suppl, ds_2)
