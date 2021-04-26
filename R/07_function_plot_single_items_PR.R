# fucntion to plot grafics for survey data PR 


#state: janaury 2021

plot_item_single_pr <- function(data, wave, question, PR = NULL, 
                             width = NULL, height = NULL, save = FALSE,
                             ncol = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (t0 or t1)
  #' @param question string containing which question of interest
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @description plots figures over all canteens for a defined data point see wave
  #' @return barplot with ggplot
  #' @export
  
  # load colors
  source("R/config_plot.R", chdir = TRUE)
  
  #prepare data
  #two conditions: 1) pr is NULL 2) pr is a string
  
  if(!is.character(PR)){
    #load information about the item categories
    if(toString(wave) == "T0"){
      #load data t0
      response_var_wave <- readr::read_delim(paste0(survey_pr, "values_mittagessen_1_2020-09-29_13-00.csv"),
                                  delim = ";", locale = locale(encoding = "latin1"),
                                  col_names = TRUE, trim_ws = TRUE, na = c("-9", ""),
                                  col_types = readr::cols(RESPONSE = readr::col_character())) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, answer = RESPONSE, item_content = MEANING) %>% 
        dplyr::select(-VAR)
    }
    else{
      #load data t1
      response_var_wave <- reader::read_delim(paste0(survey_pr, "values_mittagessen_2_2020-11-30_16-32.csv"),
                                      delim = ";", locale = locale(encoding = "UTF-8"), # file was saved differently than the file above
                                      col_names = TRUE, trim_ws = TRUE, na = c("-9", ""),
                                      col_types = readr::cols(RESPONSE = readr::col_character())) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, answer = RESPONSE, item_content = MEANING)
      }
    
    
    #plot figure if no PR is defined
    d <- data %>% 
      dplyr::filter(time == toString(wave) & questions == toString(question)) %>% 
      dplyr::group_by(PR, answer) %>% 
      dplyr::summarise(tot = n()) %>% 
      dplyr::mutate(pct = tot / sum(tot)) %>%
      dplyr::ungroup() %>% 
      dplyr::left_join(., response_var_wave[response_var_wave$questions == question, ],
                by = "answer")
    
    
    # add text for xlab
    txt <- d %>% 
      dplyr::group_by(PR) %>% 
      dplyr::summarise(tot_pr = sum(tot)) %>% 
      dplyr::mutate(xlab_ = paste0("(", tot_pr, ")"),
             xlab = paste(PR, xlab_, sep = "\n"))
    
    #merge data back
    d <- d %>% 
      dplyr::left_join(., txt, by = "PR") %>% 
      tidyr::drop_na(PR) # %>% 
      #in case answer is na => change to nicht beantwortet
      # mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer))
    
    #https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
    p <- ggplot2::ggplot(d, ggplot2::aes(x = xlab, y = pct, 
                       fill = factor(item_content,
                                     levels = pattrn, # define order
                                     labels = attributes(pal)$names))) + # define new labels if necessary
      ggplot2::geom_bar(stat = "identity", position = position_stack(reverse = T), width = .6) +
      ggplot2::scale_fill_manual(values = pal) + # define color palette, otherwise set NULL
      scale_y_origin(labels = scales::percent) +
      ggplot2::guides(fill = guide_legend("", ncol = ncol)) +
      ggplot2::geom_text(aes(label = if_else(pct < .02, "", scales::percent(pct, accuracy = 1))), 
                position = position_stack(reverse = T, vjust = .5), size = 22*converter,
                family = theme_get()$text$family) +
      ggplot2::labs(y = "", x = "") +
      ggplot2::coord_flip() +
      mytheme +
      ggplot2::theme(legend.position = "bottom")
    
  }else{
    # load information about the items, which depends which wave we are looking at
    if(toString(wave) == "T0"){
      # load information about the items to keep
      keep_var_wave <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen1_201124.csv"),
                                   delim = ";", locale = readr::locale(encoding = "latin1"),
                                   col_names = TRUE, trim_ws = TRUE) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, items = QUESTION)
      
      #load information about the labels behind the item-response
      response_var_wave <- readr::read_delim(paste0(survey_pr, "values_mittagessen_1_2020-09-29_13-00.csv"),
                                      delim = ";", locale = locale(encoding = "latin1"),
                                      col_names = TRUE, trim_ws = TRUE, na = c("-9", ""),
                                      col_types = readr::cols(RESPONSE = readr::col_character())) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, answer = RESPONSE, item_content = MEANING) %>% 
        dplyr::select(-VAR)
      
    }else{
      keep_var_wave <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen2_201124.csv"),
                                   delim = ";",
                                   col_names = TRUE) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, items = QUESTION)
      
      response_var_wave <- readr::read_delim(paste0(survey_pr, "values_mittagessen_2_2020-11-30_16-32.csv"),
                                      delim = ";", locale = locale(encoding = "UTF-8"), # file was saved differently than the file above
                                      col_names = TRUE, trim_ws = TRUE, na = c("-9", ""),
                                      col_types = readr::cols(RESPONSE = readr::col_character())) %>% 
        dplyr::filter(!is.na(VAR_CONTENT)) %>% 
        dplyr::rename(questions = VAR_CONTENT, answer = RESPONSE, item_content = MEANING)
    }
    
    
    #preapre data when PR is defined
    d <- data %>% 
      dplyr::filter(time == toString(wave)) %>% 
      dplyr::filter(stringr::str_detect(.$questions, pattern = question)) %>% 
      dplyr::filter(str_detect(.$PR, pattern = pr)) %>% 
      dplyr::group_by(questions, answer) %>% 
      dplyr::summarise(tot = n()) %>% 
      dplyr::mutate(pct = tot / sum(tot)) %>%
      dplyr::ungroup() %>% 
      #change na's to keine angaben
      dplyr::mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
      dplyr::left_join(., keep_var_wave[, c("questions", "items")], by = "questions")
    
    # add text for xlab
    txt <- d %>% 
      dplyr::group_by(items) %>% 
      dplyr::summarise(tot_pr = sum(tot)) %>% 
      dplyr::mutate(xlab_ = paste0("(", tot_pr, ")"),
             xlab = paste(items, xlab_, sep = "\n"))
    
    #merge data back
    d <- d %>% 
      dplyr::left_join(., txt, by = "items") %>% 
      tidyr::drop_na(PR)
    
    
    p <- ggplot2::ggplot(d, ggplot2::aes(x = xlab, y = pct, fill = factor(answer, levels = pattrn))) +
      ggplot2::geom_bar(stat = "identity", position = position_stack(reverse = F), width = .6) +
      ggplot2::scale_fill_manual(values = pal) + 
      scale_y_origin(labels = scales::percent) +
      ggplot2::guides(fill = ggplot2::guide_legend("")) +
      ggplot2::geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
                position = ggplot2::position_stack(reverse = F, vjust = .5), size = 22*converter,
                family = theme_get()$text$family) +
      ggplot2::labs(y = "", x = "") +
      mytheme 
    
  }
  
  if(save == TRUE){
    ggplot2::ggsave(paste0(survey_plot_pr, question, wave, "_", 
                                       format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
           plot = p,
           width = width,
           height = height,
           device = cairo_pdf)
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "PDF"," has been saved")
    
    ggplot2::ggsave(paste0(survey_plot_pr, question, wave, "_", 
                                       format(Sys.Date(), "%Y_%m_%d"), ".png"),
           plot = p,
           width = width,
           height = height,
           device = "png")
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "png"," has been saved")
    
  }else{
    print(p)
  }
  
}


#function plots over both time points
plot_item_single_both_pr <- function(data, question, PR = NULL, 
                                     tit_guide = "Menüinhalt",
                                     save = FALSE, width = NULL, height = NULL){
  
  
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param question string containing which question of interest
  #' @param tit_guide string containg title of the guide legend, default = "Menüinhalt"
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @description plots figures over all canteens for both data points
  #' @return barplot with ggplot
  #' @export
  
  # load colors
  source("R/config_plot.R", chdir = TRUE)
  
  #load infos about the items
  response_var_wave <- readr::read_delim(paste0(survey_pr, "values_mittagessen_2_2020-11-30_16-32.csv"),
                                  delim = ";", locale = locale(encoding = "UTF-8"),
                                  col_names = TRUE, trim_ws = TRUE, na = c("-9", ""),
                                  col_types = readr::cols(RESPONSE = readr::col_character())) %>% 
    dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(questions = VAR_CONTENT, answer = RESPONSE, item_content = MEANING) %>% 
    dplyr::select(-VAR)
  
  
  #prepare data
  d <- data %>% 
    dplyr::filter(questions == toString(question)) %>% 
    dplyr::group_by(PR, time, answer) %>% 
    tidyr::drop_na(answer) %>% 
    dplyr::summarise(tot = n()) %>% 
    dplyr::mutate(pct = tot / sum(tot)) %>%
    dplyr::ungroup() %>% 
    dplyr::left_join(., response_var_wave[response_var_wave$questions == question, ],
              by = "answer") %>% 
    dplyr::mutate(PR = if_else(PR == "PR1", "PR1 (K)", PR))
  
  
  # add text for xlab
  dt_p <- d %>% 
    dplyr::group_by(PR, time) %>% 
    dplyr::summarise(tot_pr = sum(tot)) %>% 
    # mutate(time_ = if_else(time == "T0", "erste Befragung", "zweite Befragung")) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot_pr,")"),
           xlab = paste(time, xlab_, sep = "\n"))  %>% 
    dplyr::left_join(d, ., by = c("PR", "time")) %>% 
    tidyr::drop_na(PR) #one person ID 304 has no canteen?
  
  #https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
  p <- ggplot2::ggplot(dt_p, aes(x = xlab, y = pct, 
                        fill = forcats::fct_rev(factor(item_content, #reverse factors to get the right order
                                              levels = attributes(pal)$names # define order
                                              )))) + # define new labels if necessary
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .6) +
    ggplot2::scale_fill_manual(values = pal, breaks = attributes(pal)$names, labels = new_lab) + # define color palette, otherwise set NULL
    scale_y_origin(labels = scales::percent) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = tit_guide, title.vjust = 1, 
                               keywidth = .8, keyheight = .8, reverse = TRUE)) +
    ggplot2::geom_text(aes(label = dplyr::if_else(pct < .02, "", scales::percent(pct, accuracy = 1))), 
              position = ggplot2::position_stack(vjust = .5), size = 22*converter,
              family = theme_get()$text$family) +
    ggplot2::labs(y = "Antworten in Prozent", x = "") +
    ggplot2::facet_grid(~ PR, scales = "free") +
    mytheme 
  
  #plot
  if(save == TRUE){
    ggplot2::ggsave(paste0(survey_plot_pr, question, "_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
           plot = p,
           width = width,
           height = height,
           device = cairo_pdf)
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            " PDF"," has been saved")
    
    ggplot2::ggsave(paste0(survey_plot_pr, question, "_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".png"),
           plot = p,
           width = width,
           height = height,
           device = "png")
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            " png"," has been saved")
    
  }else{
    print(p)}
}
  
  



plot_both_pr <- function(data, question, 
                                     tit_guide = "",
                                     save = FALSE, width = NULL, height = NULL){
  
  
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param question string containing which question of interest
  #' @param tit_guide string containg title of the guide legend
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @description plots a specific question concerning one dimension
  #' @return barplot with ggplot
  #' @export
  
  # load colors
  source("R/config_plot.R", chdir = TRUE)
  
  
  #prepare data
  d <- data %>% 
    dplyr::filter(questions == toString(question)) %>% 
    dplyr::group_by(PR, time, answer) %>% 
    tidyr::drop_na(answer) %>% 
    dplyr::summarise(tot = n()) %>% 
    dplyr::mutate(pct = tot / sum(tot)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(PR = if_else(PR == "PR1", "PR1 (K)", PR))
  
  
  # add text for xlab
  dt_p <- d %>% 
    dplyr::group_by(PR, time) %>% 
    dplyr::summarise(tot_pr = sum(tot)) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot_pr,")"),
           xlab = paste(time, xlab_, sep = "\n"))  %>% 
    dplyr::left_join(d, ., by = c("PR", "time")) %>% 
    tidyr::drop_na(PR)  #one person ID 304 has no canteen
    
  
  #https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
  p <- ggplot2::ggplot(dt_p, ggplot2::aes(x = xlab, y = pct, 
                        fill = forcats::fct_rev(factor(answer, #reverse factors to get the right order
                                                       levels = attributes(pal)$names # define order
                        )))) + # define new labels if necessary
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .6) +
    ggplot2::scale_fill_manual(values = pal, breaks = attributes(pal)$names) + # define color palette, otherwise set NULL
    scale_y_origin(labels = scales::percent) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = tit_guide, reverse = TRUE, keywidth = .8, keyheight = .8)) +
    ggplot2::geom_text(aes(label = dplyr::if_else(pct < .02, "", scales::percent(pct, accuracy = 1))), 
              position = ggplot2::position_stack(vjust = .5), size = 22*converter,
              family = theme_get()$text$family) +
    ggplot2::labs(y = "Antworten in Prozent", x = "") +
    ggplot2::facet_grid(~ PR, scales = "free") +
    mytheme 
  
  #plot
  if(save == TRUE){
    ggplot2::ggsave(paste0(survey_plot_pr, question, "_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
           plot = p,
           width = width,
           height = height,
           device = cairo_pdf)
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            " PDF"," has been saved")
    
    ggplot2::ggsave(paste0(survey_plot_pr, question, "_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".png"),
           plot = p,
           width = width,
           height = height,
           device = "png")
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            " png"," has been saved")
    
  }else{
    print(p)}
}


message("functions:\n

        - plot_item_single_pr
        - plot_item_single_both_pr
        - plot_environmental_both_pr
        
        \n are ready")
