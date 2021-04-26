#load till data data ASZ
#author: gian-andrea egeler
#date: december 2020


## ----setup----------------------------------------------------------------------------------------------------------------------------

# load functions
source("R/05_function_read_menu_data_AZ.R", echo = FALSE)
source("R/05_function_read_menu_wochenhit_data_AZ.R", encoding = "utf-8") #not sure why encoding does the trick?
source("R/05_function_read_selling_data_AZ.R")
source("R/05_function_edit_menu_data_AZ.R")

#load path
source("R/config_path.R")

# some problems:
#every thursday is vegi day => how to account for that: best way is to insert information into the documentation
#merge information from wochenhit

## ----load AZ1-------------------------------------------------------------------------------------------------------------------------------------------
# define ASZ
ASZ = "AZ1"

#define path to the files
files = paste0(till_asz, ASZ)

# read data AZ1
AZ1 <-
  list.files(path = files, pattern = ".xlsx", full.names = T) %>% 
  purrr::map_df(~read_xlsx_custom_asz(., ASZ = ASZ, range ="A12:I24")) 

#small edits
#attention every thursday was vegiday? if so the menu1 needs to be changes to menu2
AZ1 <- AZ1 %>%   
  filter(stringr::str_detect(meal_line, "Spezial|abwesend", negate = TRUE)) %>% #drop special comments
  mutate(tot_sold = as.numeric(tot_sold)) %>% 
  #add empty variable for meal label
  mutate(meal_label = NA_character_) %>% 
  #change meal_line for merging
  mutate(meal_line = case_when(meal_line == "Menü 1" ~ "menu1",
                               meal_line == "Menü 2" ~ "menu2",
                               meal_line == "Wochenhit" ~ "wochenhit",
                               TRUE ~ meal_line)) %>% 
  # add vegiday also for the days before the interventions
  mutate(day = lubridate::wday(date,
                               label = TRUE,
                               abbr = FALSE,
                               week_start = getOption("lubridate.week.start", 1))) %>%
  # vegi day before intervention, was Wednesday (somehow more than two conditions were not possible)
  mutate(meal_label = case_when(day ==  "Dienstag" &
                                  meal_line == "menu1" &
                                  date < "2020-09-14" ~ "vegetarian", # 2020-09-14 was the start of the intervention
                                TRUE ~ meal_label)) %>%
  mutate(meal_label = case_when(day ==  "Dienstag" &
                                  meal_line == "menu1" &
                                  date > "2020-10-26" ~ "vegetarian", # 2020-10-25 was the end of the intervention
                                TRUE ~ meal_label)) %>%
  # #vegi day during intervention & after intervention the same
  mutate(meal_label = case_when(day ==  "Donnerstag" &
                                  meal_line == "menu1" &
                                  date > "2020-09-13" &
                                  date < "2020-10-25" ~ "vegetarian",
                                TRUE ~ meal_label)) %>%
  #and the rest
  mutate(meal_label = case_when(meal_line == "menu1" & is.na(meal_label) ~ "meat",
                                meal_line == "wochenhit" & is.na(meal_label) ~ "meat",
                                meal_line == "menu2" & is.na(meal_label) ~ "vegetarian",
                                TRUE ~ meal_label)) %>% 
  drop_na(tot_sold)



## ----load AZ2---------------------------------------------------------------------------------------------------------------------------------------------------
# define ASZ
ASZ = "AZ2"

#define path to the files
files = paste0(till_asz, ASZ)

# read data AZ2
AZ2 <-
  list.files(path = files, pattern = ".xlsx", full.names = T) %>% 
  purrr::map_df(~read_xlsx_custom_asz(., ASZ = ASZ, range ="A14:I25")) 



#small edits
AZ2 <- AZ2 %>%   
  filter(stringr::str_detect(meal_line, "comments|abwesend", negate = TRUE)) %>% #drop spezial comments
  mutate(tot_sold = as.numeric(tot_sold)) %>%
  #add empty variable for meal label
  mutate(meal_label = NA_character_) %>% 
  #change meal_line for merging
  mutate(meal_line = case_when(meal_line == "Menü 1" ~ "menu1",
                               meal_line == "Menü 2" ~ "menu2",
                               meal_line == "Wochenhit" ~ "wochenhit",
                               TRUE ~ meal_line)) %>% 
  # add vegiday also for the days before the interventions
  mutate(day = lubridate::wday(date,
                               label = TRUE,
                               abbr = FALSE,
                               week_start = getOption("lubridate.week.start", 1))) %>%
  # vegi day before and after intervention, was Wednesday (somehow more than two conditions were not possible)
  mutate(meal_label = case_when(day ==  "Mittwoch" &
                                meal_line == "menu1" &
                                date < "2020-09-14" ~ "vegetarian", # 2020-09-14 was the start of the intervention
                                TRUE ~ meal_label)) %>%
  mutate(meal_label = case_when(day ==  "Mittwoch" &
                                meal_line == "menu1" &
                                date > "2020-10-25" ~ "vegetarian", # 2020-10-25 was the end of the intervention
                                TRUE ~ meal_label)) %>%
  #and the rest
  mutate(meal_label = case_when(meal_line == "menu1" & is.na(meal_label) ~ "meat",
                                meal_line == "wochenhit" & is.na(meal_label) ~ "meat",
                                meal_line == "menu2" & is.na(meal_label) ~ "vegetarian",
                                TRUE ~ meal_label)) %>% 
  # delete info about intervention => is beeing loaded separately
  mutate(meal_label = case_when(meal_line == "menu1" &
                                  date > "2020-09-13" &
                                  date < "2020-10-26" ~ NA_character_,
                                TRUE ~ meal_label)) %>%
  mutate(meal_label = case_when(meal_line == "menu2" &
                                  date > "2020-09-13" &
                                  date < "2020-10-26" ~ NA_character_,
                                TRUE ~ meal_label)) %>% 
  
  drop_na(tot_sold)




## ----load AZ3------------------------------------------------------------------------------------------------------------------------------------------------
# define ASZ
ASZ = "AZ3"

#define path to the files
files = paste0(till_asz, ASZ)

# read data AZ3
AZ3 <-
  list.files(path = files, pattern = ".xlsx", full.names = T) %>% 
  purrr::map_df(~read_xlsx_custom_asz(., ASZ = ASZ, range = "A11:I27"))

#some edits
AZ3 <- AZ3 %>%   
  filter(stringr::str_detect(meal_line, "Spezial|Abwesend|m1_vegi|m1_fleisch|m1_nur_fleisch|wochenhit_vegi|Schonkost", negate = TRUE)) %>% #drop spezial comments
  mutate(tot_sold = as.numeric(tot_sold)) %>% 
  #add empty variable for meal label
  mutate(meal_label = NA_character_) %>% 
  #change meal_line for merging
  mutate(meal_line = case_when(meal_line == "Menü 1" ~ "menu1",
                               meal_line == "Menü 2" ~ "menu2",
                               meal_line == "Wochenhit" ~ "wochenhit",
                               TRUE ~ meal_line)) %>% 
  # add vegiday also for the days before the interventions
  mutate(day = lubridate::wday(date,
                               label = TRUE,
                               abbr = FALSE,
                               week_start = getOption("lubridate.week.start", 1))) %>%
  # vegi day before intervention, was Tuesday
  mutate(meal_label = case_when(day ==  "Dienstag" &
                                  meal_line == "menu1" &
                                  date < "2020-09-14" ~ "vegetarian", # was the start of the intervention
                                  TRUE ~ meal_label)) %>%
 
  # #vegi day during intervention & after intervention the same
  mutate(meal_label = case_when(day ==  "Donnerstag" &
                                  meal_line == "menu1" &
                                  date > "2020-09-13" &
                                  date < "2020-11-09" ~ "vegetarian",
                                TRUE ~ meal_label)) %>%
  #and the rest
  mutate(meal_label = case_when(meal_line == "menu1" & is.na(meal_label) ~ "meat",
                                meal_line == "wochenhit" & is.na(meal_label) ~ "meat",
                                meal_line == "menu2" & is.na(meal_label) ~ "vegetarian",
                                meal_line == "schonkost" & is.na(meal_label) ~ "vegetarian",
                                TRUE ~ meal_label)) %>% 
  drop_na(tot_sold)
  
  


# define place
ASZ = "AZ3/amusebouche"

#define path to the files
files = paste0(till_asz, ASZ)

# read data amisbouche => problem with date!
AZ3_amuse <-
  list.files(path = files, pattern = ".xlsx", full.names = T) %>% 
  purrr::map_df(~read_xlsx_custom_asz(., ASZ = ASZ, range = "A4:I14")) # throws an error, due to change from numeric to date (if no number there, error occurs)


# some small edits
AZ3_amuse <- AZ3_amuse %>%   
  filter(stringr::str_detect(info_amis, "Besonderes", negate = TRUE)) %>% #drop spezial comments
  mutate(tot = as.numeric(tot)) %>% 
  drop_na(tot)


## ----load AZ4-------------------------------------------------------------------------------------------------------------------------------------------------
# define ASZ
ASZ = "AZ4"

#define path to the files
files = paste0(till_asz, ASZ)

# read data AZ4
AZ4 <-
  list.files(path = files, pattern = ".xlsx", full.names = T) %>% 
  purrr::map_df(~read_xlsx_custom_asz(., ASZ = ASZ, range = "A13:I89")) 
  
AZ4 <- AZ4 %>% 
  rename(table_info = meal_line, meal_line = meal_port) %>% 
  drop_na(meal_line) %>% # drop all weekdays in tot_sold
  mutate(tot_sold = as.numeric(tot_sold)) %>% 
  #add empty variable for meal label
  mutate(meal_label = NA_character_) %>% 
  #change meal_line for merging
  mutate(meal_line = case_when(meal_line == "Menü 1" ~ "menu1",
                               meal_line == "Menu 2" ~ "menu2",
                               meal_line == "Wochenhit" ~ "wochenhit",
                               TRUE ~ meal_line)) %>% 
  # add vegiday also for the days before the interventions
  mutate(day = lubridate::wday(date,
                               label = TRUE,
                               abbr = FALSE,
                               week_start = getOption("lubridate.week.start", 1))) %>%
  # vegi day before & during intervention was Thursday
  mutate(meal_label = case_when(day ==  "Donnerstag" &
                                meal_line == "menu1" ~ "vegetarian",
                                TRUE ~ meal_label)) %>%
  #and the rest
  mutate(meal_label = case_when(meal_line == "menu1" & is.na(meal_label) ~ "meat",
                                meal_line == "wochenhit" & is.na(meal_label) ~ "meat",
                                meal_line == "menu2" & is.na(meal_label) ~ "vegetarian",
                                TRUE ~ meal_label)) %>% 
  drop_na(tot_sold)



## ----load meal plan------------------------------------------------------------------------------------------------------------------------------------------------
#"normal" menus
menu <-
  list.files(path = paste0(till_asz, "menuplan_asz") , 
             pattern = "*.docx", full.names = T) %>% 
  purrr::map(~docxtractr::read_docx(.)) %>%
  purrr::map_df(~extr_table_doc(.), .id = "data_source")   # id returns only the index of the list from above


#wochenhit: somehow sourcing the function is not working => not sure where the problems lies
wochenhit <- 
  list.files(path = paste0(till_asz, "menuplan_asz"),
             pattern = "*.docx", full.names = T) %>% 
  purrr::map(~ docxtractr::read_docx(.))  %>% 
  map_df(~ extr_wochenhit_doc(.))
  


# house keeping => case_when wont work, why!!?
menu_2020 <- house_keeping_asz(menu) 
wochenhit <- house_keeping_asz(wochenhit)


# load information about az2 intervention
intervention_AZ2 <- read_delim(paste0(till_asz, "AZ2/intervention_AZ2.csv"),
                                  delim = ";",
                                  col_types = cols(date = col_date(format = "%d.%m.%Y")),
                                  locale = locale(encoding = "latin1")) %>% 
  mutate(meal_line = case_when(meal_line == "Menü 1" ~ "menu1",
                               meal_line == "Menü 2" ~ "menu2",
                               TRUE ~ meal_line)) 


# rename meal_label content: not working in the function
menu_2020 <- menu_2020 %>% 
  mutate(meal_line = case_when(meal_line == "Mittags.Menü.1" ~ "menu1",
                                      meal_line == "Mittags.Menü.2" ~ "menu2",
  
                                    TRUE ~ meal_line)) %>% 
  # add day for vegiday
  mutate(day = lubridate::wday(date,
                              label = TRUE,
                              abbr = FALSE,
                              week_start = getOption("lubridate.week.start", 1))) %>% 
  mutate(meal_label = case_when(meal_line == "menu1" ~ "meat",
                                meal_line == "wochenhit" ~ "meat",
                                meal_line == "menu2" ~ "vegetarian")) %>% 
  mutate(meal_label = case_when(day ==  "Donnerstag" & meal_line == "menu1" ~ "vegetarian",
                                TRUE ~ meal_label)) %>%  # attention R language is german
  mutate(kw = isoweek(date)) %>% 
  select(-day, -meal_label)  #%>% 
  # full_join(., wochenhit)


##------------merge menu info of the intervention and info of intervention AZ2-------------------------------------------------
df_till <- bind_rows(AZ1, AZ3, AZ4) %>% 
  left_join(., menu_2020, by = c("date", "meal_line", "kw")) %>% 
  # problem merging data correct, i guess du to NA => no solution found yet
  left_join(., wochenhit, by = c("meal_line", "kw")) %>% 
  # thus coalesce the info from both meal_labels into one and drop them
  mutate(meal_content = coalesce(.$meal_content.x, .$meal_content.y)) %>% 
  select(-meal_content.x, -meal_content.y)

  
#filter meal_label which is not in the intervention
df_AZ2 <- AZ2 %>% 
  # merging information is somehow not working, due to NA
  left_join(., intervention_AZ2, by = c("meal_line", "date")) %>% 
  #thus coalesce the info from both meal_labels into one and drop them
  mutate(meal_label = coalesce(.$meal_label.x, .$meal_label.y)) %>% 
  select(-meal_label.x, -meal_label.y) %>% 
  # add wochenhit
  left_join(., wochenhit, by = c("meal_line", "kw")) %>% 
  # coalesce the info from both meal_labels into one and drop them
  mutate(meal_content = coalesce(.$meal_content.x, .$meal_content.y)) %>% 
  select(-meal_content.x, -meal_content.y)
  


# last step merge information about the wochenhit
df_till_ <- bind_rows(df_AZ2, df_till) 



##------------merge info with selling dat, last edits---------------------------------------------------

# paste data again togheter and merge with infos from above
df_till_asz <- df_till_ %>% 
  #group information again, because of the proportion sizes (we dont account for them)
  group_by(date, meal_line, meal_label, meal_content, ASZ) %>% 
  summarise(tot_sold = sum(tot_sold, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # set 0 to NA and drop them all
  mutate(tot_sold = if_else(tot_sold == 0, NA_real_, tot_sold)) %>% 
  drop_na(tot_sold) %>% 
  mutate(kw = isoweek(date)) %>% 
  # select only week, which have all in common: 36-44
  filter(kw != 35 & kw != 45) %>% 
  #add new variabel with info to the interventionphase
  mutate(condit = NA_character_,
         condit = case_when(date <= "2020-09-13" ~ "Vorher",
                            TRUE ~ condit)) %>%
  mutate(condit = case_when(date >= "2020-10-26" ~ "Nachher",
                            TRUE ~ condit)) %>% 
  mutate(condit = case_when(date > "2020-09-13" &
                            date < "2020-10-26" ~ "Intervention",
                            TRUE ~ condit)) 


  
#drop files
rm(AZ1, AZ2, AZ3, AZ4, menu, df_till_, 
   df_till, files, ASZ, df_AZ2)
