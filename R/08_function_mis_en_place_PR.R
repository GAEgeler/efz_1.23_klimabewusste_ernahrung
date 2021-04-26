# mis en place 

#required libraries
library(stringr)
library(stringi)


house_keeping_pr <- function(data){
  
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) without umlaute, asterics ect.
  #' @export
  
  # house keeping
  data$article_description <-  stringi::stri_trans_general(data$article_description,"Latin-ASCII") # replace all umlaute
  data$article_description <- stringr::str_to_lower(data$article_description) # all to lower capitals
  data$article_description <-  stringr::str_replace(data$article_description, '\\*', '') # replace all asterisks
  data$article_description <- stringr::str_replace_all(data$article_description, "[[:punct:]]", " ") # remove all punctuation
  data$article_description <- stringr::str_trim(data$article_description, side = "both") #delete whitespace at the beginning and ending
  data$article_description <- stringi::stri_replace_all_fixed(data$article_description, " ", "") #delete all whitespace
  
  
  return(data)
}


mis_en_place_pr3 <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) without umlaute, asterics ect.
  #' @description returns cleaned data set containing only lunch meals
  #' @export
 
  #housekeeping function
  data <- house_keeping_pr(data)
  
  #drop some weird entries in article_id
  data <- data %>%
    dplyr::filter(stringr::str_detect(.$article_id,"Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Gruppentotal|Artikel", negate = TRUE)) %>% 
    dplyr::mutate(article_id = as.integer(.$article_id))

  #edit some article_descriptions
  data <- data %>% 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$article_description, "12portion|12port", "")) %>% # whatch out for whithe space
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "vegetarisch", "menu3")) %>%  # change vegetarisch to menu 3 (according menu offer)
    #pay attention of the order of replacements, especially when the replacement is a sub of another replacement e.g. sallatteller and salattellerklein
    dplyr::mutate(meal_line = stringi::stri_replace_all_regex(.$meal_line, pattern = "salattellerklein|salattakeaway|araglattlunchbuffet|salatteller", replacement = "buffet", vectorize_all = TRUE))
  
  #filter only lunch meals
  pat = c("menu1|menu2|menu3|buffet")  
  data <- data %>% 
    filter(stringr::str_detect(.$meal_line, pattern = pat))
  
  paste("unique articles in dataset: ", unique(data$meal_line))
  
  return(data)
}




mis_en_place_pr4 <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) withoout umlaute, asterics ect.
  #' @description returns cleaned data set of pr4 containing only lunch meals
  #' @export
  
  #housekeeping function
  data <- house_keeping_pr(data)
  
  #drop some weird entries in article_id
  data <- data %>%
    dplyr::filter(stringr::str_detect(.$article_id,"Montag|Dienstag", negate = TRUE)) %>% 
    dplyr::mutate(article_id = as.integer(.$article_id))
  
  #edit some article_descriptions
  data <- data %>% 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$article_description, "tagesmenu1", "menu1")) %>%
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "ewzhit", "menu2")) %>%  # ewz hit is like a second meal line, with meat
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "vegimenusammelpass|vegimenu", "menu3")) %>%  # change vegetarisch to menu 3 
    #pay attention of the order of replacements, especially when the replacement is a sub of another replacement e.g. sallatteller and salattellerklein
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "salatbuffet", "buffet"))
  
  #filter only lunch meals
  pat = c("menu1|menu2|menu3|buffet")  
  data <- data %>% 
    dplyr::filter(stringr::str_detect(.$meal_line, pattern = pat))
  
  paste("unique articles in dataset: ", unique(data$meal_line))
  
  return(data)
  
  
}


mis_en_place_pr2 <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) withoout umlaute, asterics ect.
  #' @description returns cleaned data set of pr2 containing only lunch meals
  #' @export
  
  #housekeeping function
  data <- house_keeping_pr(data) 
  
  #drop some weird entries in article_id
  data <- data %>%
    dplyr::filter(stringr::str_detect(.$article_id,"Dienstag|Freitag|Artikel|Gruppentotal", negate = TRUE)) %>% 
    dplyr::mutate(article_id = as.integer(.$article_id))
  
  
  #edit some article_descriptions
  data <- data %>% 
    #attention order of replacements, see problem above
    dplyr::mutate(meal_line = stringr::str_replace_all(.$article_description, "menuii", "menu3")) %>% 
    # mutate(article_description = stringr::str_replace_all(.$article_description, "gemuseundbeilage", "menu2")) %>% # is like another meal line => cost less (7-8 chf) 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "menui", "menu1")) %>%  # change vegi line to menu 3 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "salatbuffet", "buffet"))
  
  
  #filter only lunch meals
  pat = c("menu1|menu2|menu3|buffet")  
  data <- data %>% 
    dplyr::filter(stringr::str_detect(.$meal_line, pattern = pat))
  
  paste("unique articles in dataset: ", unique(data$meal_line))
  
  
  return(data)
  

}



mis_en_place_pr1_1 <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) without umlaute, asterics ect.
  #' @description returns cleaned data set of pr1 containing only lunch meals
  #' @export
  
  #housekeeping function
  data <- house_keeping_pr(data)
  
  # change some meals
  #menuperso04 cannot be take as meal_line, because it contains meat and veg meals
  data <- data %>% 
    dplyr::mutate(meal_line = stringi::stri_replace_all_regex(.$article_description, "menufleischfisch|tagesteller1", "menu1")) %>%
    dplyr::mutate(meal_line = stringi::stri_replace_all_regex(.$meal_line, "menuvegetarisch|tagesteller2", "menu3")) %>%
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "menuohnefleisch", "menu1_veg")) %>% #vegetarian
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "fitnesstellerfleisch", "menu2")) %>% 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line,
                                                                 pattern = "buffettakeaway|salat|kalterteller", 
                                                                 replacement = "buffet"))

    
  #filter only lunch meals
  pat = c("menu1|menu2|menu3|^buffet$")  
  data <- data %>% 
    dplyr::filter(stringi::stri_detect_regex(.$meal_line, pattern = pat)) %>% 
    dplyr::filter(stringi::stri_detect_regex(.$meal_line, "buffetzummenu13", negate = TRUE))  #does somehow comes with => no sure why
    
  paste("unique articles in dataset: ", unique(data$meal_line))
  
  return(data)
  
}


mis_en_place_pr1_2 <- function(data){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @return cleaned data (df) without umlaute, asterics ect.
  #' @description returns cleaned data set of pr1 containing only lunch meals
  #' @export
  
  #housekeeping function
  data <- house_keeping_pr(data)
  
  #drop some weird entries in article_id
  if("article_id" %in% names(data)){
    
    data <- data %>%
      dplyr::filter(stringr::str_detect(.$article_id,"Total|Montag|Donnerstag|Freitag|Artikel", negate = TRUE)) %>% 
      dplyr::mutate(article_id = as.integer(.$article_id))} #%>% 
  # drop_na(tot_sold)
  
  
  # change some meals
  #menuperso04 cannot be take as meal_line, because it contains meat and veg meals
  data <- data %>% 
    dplyr::mutate(meal_line = stringi::stri_replace_all_regex(.$article_description, "menufleischfisch|tagesteller1", "menu1")) %>%
    dplyr::mutate(meal_line = stringi::stri_replace_all_regex(.$meal_line, "menuvegetarisch|tagesteller2", "menu3")) %>%
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "menuohnefleisch", "menu1_veg")) %>% #vegetarian
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line, "fitnesstellerfleisch", "menu2")) %>% 
    dplyr::mutate(meal_line = stringr::str_replace_all(.$meal_line,
                                                          pattern = "buffettakeaway|salat|kalterteller", 
                                                          replacement = "buffet"))
  
  
  #filter only lunch meals
  pat = c("menu1|menu2|menu3|^buffet$")  
  data <- data %>% 
    dplyr::filter(stringi::stri_detect_regex(.$meal_line, pattern = pat)) %>% 
    dplyr::filter(stringi::stri_detect_regex(.$meal_line, "buffetzummenu13", negate = TRUE))  #does somehow comes with => no sure why
  
  paste("unique articles in dataset: ", unique(data$meal_line))
  
  return(data)
  
}




#function to add meal content resp. meal_label
add_meal_label_pr <- function(data){
  #' @author gian-andrea egeler, january 2021
  #' @param data data set (as data frame or tibble)
  #' @return data (df) with new variable meal_label containing meat, vegetarian, buffet
  #' @export
  
  #adding new variable meal label
  data <- data %>% 
    dplyr::mutate(meal_label = dplyr::case_when(meal_line == "menu1" ~ "meat",
                                  meal_line == "menu2" ~ "meat",
                                  meal_line == "menu3" ~ "vegetarian",
                                  meal_line == "menu1_veg" ~ "vegetarian",
                                  TRUE ~ meal_line))
  
  return(data)
  
}



message("functions: \n
        - house_keeping_pr
        - mis_en_place_pr3
        - mis_en_place_pr4
        - mis_en_place_pr2
        - mis_en_place_pr1 (x2)
        - add_meal_label_pr \n
        are ready!")