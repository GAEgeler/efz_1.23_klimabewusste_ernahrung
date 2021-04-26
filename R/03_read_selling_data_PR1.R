# read meal menus from PR1 (staff canteen)--------

# status: march 2020
# author: gian-andrea egeler
# note: check out readme.txt

# libraries
library(tidyverse)
library(readxl)
library(here) # pay attention with there is a conflict with lubridate!
# library(pdftools)
library(lubridate)

#load path infos
source("R/config_path.R")


# #read data 2019-------------------
tab_names <- excel_sheets(paste0(till_pr, "PR1/2019/Artikelanalyse_April_Juni_2019_egel.xlsx"))
list_all <- lapply(tab_names, function(x) read_excel(paste0(till_pr, "PR1/2019/Artikelanalyse_April_Juni_2019_egel.xlsx"),
                                                    sheet = x,  
                                                    range = "A4:AF24", col_types = "guess"))

#unlist data: warnings can be ignored
selling <- tibble()
for (i in 1:length(list_all)) {
  # subset data
  # errors can be ignored => due to empty coloms while reading excel sheets
  dt <- plyr::ldply(list_all[i], data.frame) %>%
    pivot_longer(-Datum, names_to = "date", values_to = "tot_sold") %>% 
    mutate(date_ = str_replace(date, "X", "")) %>% 
    mutate(date_2 = as_date(as.numeric(.$date_), origin = "1899-12-30")) %>% # extract date https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r 
    drop_na(tot_sold) %>% # drop na's in tot_sold
    select(date_2,  Datum, tot_sold) %>% 
    rename(date= date_2, meal_line = Datum)
  
  # merge them together
  selling <- bind_rows(dt, selling)
}

# filter only important data
pat = c("Menü Fleisch|Menü Vegetarisch") # there could be more to merge e.g. salat; büffet take away

selling_1 <- selling %>% 
  filter(str_detect(.$meal_line, pat)) %>% 
  mutate(meal_line = str_replace(.$meal_line, " / Fisch", "")) %>% 
  mutate(meal_line = str_replace_all(.$meal_line, c("Menü Fleisch" = "menu1", "Menü Vegetarisch" = "menu2")))

                     
# load info from menu offer
source("R/03_read_menu_data_PR1.R")


# merge
# somehow an error is occurring
selling_PR1 <- selling_1 %>% 
  left_join(menu_offer_pr1, ., by = c("date", "meal_line")) %>% 
  filter(meal_line != "ABENDESSEN") %>% 
  mutate(meal_content = str_trim(.$meal_content, side = "both")) %>%
  mutate(source = "PR1") %>%
  select(date, meal_line, tot_sold, meal_content, meal_label, source) %>% 
  rename(meal_component = meal_content) # due to issue, see https://github.zhaw.ch/egel/menu_till_data_stadt_zh/issues/1


# delete all unsused datasets
rm(list = c("dt", "selling", "selling_1", "pat", "tab_names", "menu_offer_pr1")) 


            