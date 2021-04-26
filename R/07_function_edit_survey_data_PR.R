# edit survey data (PR) --------

# status: november 2020
# author: gian-andrea egeler

edit_survey_pr <- function(data, wave = NULL){
  #' @param data is a data frame to edit
  #' @param wave a string wich contains two conditions first or second wave
  #' @return a list containing two datasets: one in the long and one in the wide form
  #' @details two filters were set: 1) drop outs with less than 3 Minutes 
  #' 2) drop out when not until the 11th page of the survey (most of it was fot testing)
  #' @details questions to choice attitude missing in the mobile version
  #' @export
  
  #load config file
  source("R/config_path.R")
  
  # supress col specification
  options(readr.num_columns = 0)
  
  #load information of variable
  #select only variable we need
  keep_var_wave1 <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen1_201124.csv"),
                               delim = ";", locale = readr::locale(encoding = "latin1"),
                               col_names = TRUE, 
                               trim_ws = TRUE) %>% 
    dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(question = VAR_CONTENT)
  
    
  #seect only variable we need
  keep_var_wave2 <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen2_201124.csv"),
                               delim = ";", locale = readr::locale(encoding = "latin1"), 
                               col_names = TRUE, trim_ws = TRUE) %>% 
    dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(question = VAR_CONTENT)
  
  
  #edit data
  if(toString(wave) == "first"){
    
    
    #############
    # filter and edit data => time should be over 190 seconds (4minutes) => and last page 10?
    ############
    
    df_wide <- data[, names(data) %in% keep_var_wave1$VAR] %>%
      # filter out missings (42 f?lle raus)
      dplyr::filter(TIME_SUM > 189) %>% # 36
      dplyr::filter(LASTPAGE > 10) %>%  # 7
      dplyr::filter(SE03_01 > 1921) %>% #drop age older than 1940
      # tidyr::drop_na(MI01) %>% #drop missings in PR
      #change var names according keep_var
      setNames(., keep_var_wave1$question) %>% 
      # add new variable called PR, important for filtering later on
      dplyr::mutate(PR = dplyr::case_when(.$shop_description == 1 ~ "PR1",
                            .$shop_description == 2 ~ "PR3",
                            .$shop_description == 3 ~ "PR2",
                            .$shop_description == 4 ~ "PR4", 
                            .$shop_description == -9 ~ NA_character_)) %>% 
      # paste together information for the follow_up
      dplyr:: mutate(identif_ = paste0(id_match1, id_match2),
             identif_2 = paste0(id_match3, id_match4),
             follow_up = dplyr::if_else(identif_ == "", NA_character_, paste(identif_,
                                                                      identif_2,
                                                                      sep = "_")),
             follow_up = stringi::stri_trans_tolower(follow_up)) %>% 
      # add age in years
      dplyr::mutate(age_years = 2020 - age) %>% 
      #drop some columns
      dplyr::select(-TIME_SUM, -LASTPAGE, -id_match1, -id_match2, -id_match3, 
             -id_match4, -identif_, -identif_2, -shop_description) %>% 
      dplyr::mutate(time = "T0") %>% 
      #drop missings in PR
      tidyr::drop_na(PR)
    
    df_long <- df_wide %>% 
     # pivot to long format is due to numerical variables not working
      tidyr::pivot_longer(-c(ID, type, date, follow_up, PR), names_to =  "questions", values_to = "answer", 
                   values_transform = list(answer = as.character)) %>% 
      dplyr::mutate(time = "T0") 
    
    
  
  }else{
    #drop first all variables not used
    df_wide <- data[, names(data) %in% keep_var_wave2$VAR] %>%
      # filter out missings (96 cases)
      dplyr::filter(TIME_SUM > 189) %>% # 95 f?lle (many of theme were from another canteen)
      dplyr::filter(LASTPAGE > 10) %>%  # 1 fall
      dplyr::filter(P103_01 > 1921) %>% #drop age older than 1940
      # tidyr::drop_na(M101) %>% #drop missings in PR
      #change var names according keep_var
      setNames(., keep_var_wave2$question) %>%
      # add new variable called PR, important for filtering later on
      dplyr::mutate(PR = dplyr::case_when(.$shop_description == 1 ~ "PR1",
                            .$shop_description == 2 ~ "PR3",
                            .$shop_description == 3 ~ "PR2",
                            .$shop_description == 4 ~ "PR4", 
                            .$shop_description == -9 ~ NA_character_)) %>% 
      #paste together identification
      dplyr::mutate(identif_ = paste0(id_match1, id_match2),
             identif_2 = paste0(id_match3, id_match4),
             follow_up = dplyr::if_else(identif_ == "", NA_character_, paste(identif_, identif_2, sep = "_")),
             follow_up = stringi::stri_trans_tolower(follow_up)) %>% 
      # add age in years
      dplyr::mutate(age_years = 2020 - age) %>% 
      # drop some columns
      dplyr::select(-TIME_SUM, -LASTPAGE, -id_match1, -id_match2, 
             -id_match3, -id_match4, -identif_, -identif_2, -shop_description) %>% 
      dplyr::mutate(time = "T1") %>% 
      #drop missings in PR
      tidyr::drop_na(PR)
      
    
    df_long <- df_wide %>%       
      tidyr::pivot_longer(-c(ID, type, date, follow_up, PR), names_to =  "questions", values_to = "answer", 
                   values_transform = list(answer = as.character)) %>% 
      dplyr::mutate(time = "T1") 
      
  }
  
  return(list(df_long, df_wide))
  
  
}


message("function edit_survey_pr is ready")