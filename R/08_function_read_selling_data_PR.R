# read data from TPC of canteens for csv & xlsx


#function for csv files
read_csv_1_custom_pr <- function(path, n = NULL,  var_date = NULL){
  
  #' @return dataset with date of all transactions per day
  #' @param path defines the path to the data
  #' @param n defines how many rows need to be skipped per file
  #' @param var_date specifies the column, where the date is
  #' @description version 1: reads each csv single file and adds date of selling, however the date is in one column
  #' @author gian-andrea egeler
  #' @examples from here https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
  #' @export
  
  # read csv
  df <- readr::read_csv(file = path, skip = n, col_names = T, trim_ws = T, 
                        locale = readr::locale(encoding = "latin1") #https://github.com/tidyverse/readr/issues/799
                        ) %>% # https://github.com/tidyverse/readr/issues/892
    janitor::clean_names() %>% 
    #there are some super special cases in pr1 with weird delimeter "\": quote = "'\"'"
    dplyr::filter(stringi::stri_detect_regex("Total Gruppe|\"Total+|\"Total Gruppe|Gruppe|Gruppen|Gesamt|Artikel",
                                      .$artikel, negate = TRUE)) %>% 
    dplyr::rename(article_description = x2, article_id = artikel, total_amount = bruttobetrag , 
           tot_sold = verkaufte_stucke, avg_price = durchschnittlicher_preis) %>% 
    dplyr::select(article_id, article_description, tot_sold, total_amount, avg_price)
  
  # extract date (not the nicest way :)
  df_date <- readr::read_csv(
    file = path,
    trim_ws = T, 
    col_names = F) %>%
    janitor::clean_names() %>% 
    #https://rlang.r-lib.org/reference/sym.html
    dplyr::select(rlang::sym(var_date)) %>% 
    dplyr::filter(str_detect(!!rlang::sym(var_date), "Datums")) %>% 
    dplyr::mutate(date = stringr::str_sub(!!rlang::sym(var_date), start = -10)) %>%
    dplyr::mutate(date = lubridate::as_date(.$date, format = "%d.%m.%Y"))
  
    
  # concatenate
  df$date = df_date$date
  
  message("data of", unique(df$date), "was read successfully.")
  
  return(df)                
  
}


#function for csv files
read_csv_2_custom_pr <- function(path, n = NULL,  var_date = NULL){
  
  #' @return dataset with date of all transactions per day
  #' @param path defines the path to the data
  #' @param n defines how many rows need to be skipped per file
  #' @description version 2 reads each csv single file and adds date of selling, were the date is in two different columns!
  #' @author gian-andrea egeler
  #' @examples from here https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once
  #' @export
  
  # read csv
  df <- readr::read_csv(file = path, skip = n, col_names = T, trim_ws = T, 
                        locale = readr::locale(encoding = "latin1", grouping_mark = ","), #https://github.com/tidyverse/readr/issues/799
  ) %>% # https://github.com/tidyverse/readr/issues/892
    janitor::clean_names() %>%           
    dplyr::filter(stringr::str_detect("Total Gruppe|Gruppe|Gruppen+|Gruppentotal|Gesamt|Artikel", .$artikel, negate = TRUE)) %>% 
    dplyr::rename(article_description = x2, article_id = artikel, total_amount = bruttobetrag , 
           tot_sold = verkaufte_stucke, avg_price = durchschnittlicher_preis) %>% 
    dplyr::select(article_id, article_description, tot_sold, total_amount, avg_price)
  
  # extract date (not the nicest way :)
  df_date <- readr::read_csv(
    file = path,
    trim_ws = T, 
    col_names = F) %>%
    janitor::clean_names() %>% 
    #https://rlang.r-lib.org/reference/sym.html
    dplyr::select(rlang::sym(var_date)) %>% 
    dplyr::slice(7) %>% 
    dplyr::mutate(date = stringr::str_sub(!!rlang::sym(var_date), start = -10)) %>%
    dplyr::mutate(date = lubridate::as_date(.$date, format = "%d.%m.%Y"))
  
  
  # concatenate
  df$date = df_date$date
  
  message("data of", unique(df$date), "was read successfully.")
  
  return(df)                
  
}

#function for xlsx files
read_xlsx_1_custom_pr <- function(path, n = NULL){
  
  #' @return dataset with date of all transactions per day
  #' @param path defines the path to the data
  #' @param n defines how many rows need to be skipped per file
  #' @description reads each single xlsx file, version 1 with different variables names, and adds date of selling
  #' @author gian-andrea egeler
  #' @export

  # read xlsx
  df <- readxl::read_xlsx(path = path, skip = n, col_names = T) %>%
    janitor::clean_names() %>%  
    dplyr::filter(stringr::str_detect("Total Gruppe|Gruppe|Gruppen|Gesamt", .$artikel, negate = TRUE)) %>% 
    dplyr::rename(article_description = x3, article_id = artikel, total_amount = brutto_betrag , 
           tot_sold = verk_stuck, avg_price = durchschn_v_preis) %>% 
    dplyr::select(article_id, article_description, tot_sold, total_amount, avg_price)
  
  # extract date (not the nicest way :)
  df_date <- readxl::read_xlsx(
    path = path,
    trim_ws = T, 
    col_names = F) %>%
    janitor::clean_names() %>% 
    select(x19) %>% 
    filter(grepl("Datums", .$x19)) %>% 
    mutate(date = stringr::str_sub(.$x19, start = -10)) %>%
    mutate(date = lubridate::as_date(.$date, format = "%d.%m.%Y")) #tz throws an error, dont kno why
  
  # concatenate
  df$date = df_date$date
  
  message("data of", unique(df$date), "was read successfully.")
  
  return(df)                
  
}


read_xlsx_2_custom_pr <- function(path, n = NULL, var_date = NULL){
  
  #' @return dataset with date of all transactions per day
  #' @param path defines the path to the data
  #' @param n defines how many rows need to be skipped per file
  #' @param var_date defines were the variable were the date is
  #' @description reads each single xlsx file version 2 and adds date of selling
  #' @author gian-andrea egeler
  #' @export
  
  # read xlsx
  df <- readxl::read_xlsx(path = path, skip = n, col_names = T) %>%
    janitor::clean_names() %>%  
    filter(stringr::str_detect("Total Gruppe|Gruppe|Gruppen|Gesamt|Artikel", .$artikel, negate = TRUE)) %>% 
    rename(article_description = x2, article_id = artikel, total_amount = bruttobetrag , 
           tot_sold = verkaufte_stucke, avg_price = durchschnittlicher_preis) %>% 
    select(article_id, article_description, tot_sold, total_amount, avg_price)
  
  # extract date (not the nicest way :)
  df_date <- readxl::read_xlsx(
    path = path,
    trim_ws = T, 
    col_names = F) %>%
    janitor::clean_names() %>% 
    #https://rlang.r-lib.org/reference/sym.html
    select(rlang::sym(var_date)) %>% 
    filter(str_detect(!!rlang::sym(var_date), "Datums")) %>% 
    mutate(date = stringr::str_sub(!!rlang::sym(var_date), start = -10)) %>%
    mutate(date = lubridate::as_date(.$date, format = "%d.%m.%Y")) #tz throws an error, dont kno why
  
  # concatenate
  df$date = df_date$date
  
  message("data of", unique(df$date), "was read successfully.")
  
  return(df)                
  
}


message("functions: \n
        - read_csv_1_custom_pr: with the date in one column
        - read_csv_2_custom_pr: with the date in two different columns 
        - read_xlsx_1_custom_pr
        - read_xlsx_2_custom_pr: with different variable names than in version 1
        - read_unique_xlsx_pr \n
        are ready!")
