# fucntion to plot grafics for survey data pr

#state: december 2020


#plot with to conditions
plot_item_t1_pr <- function(data,
                            question, 
                            pr = NULL,
                            ylab = NULL,
                            title = NULL,
                            wrap = NULL,
                            save = FALSE, width = NULL, height = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param PR string containing information of the staff restaurant
  #' @param question list of a test battery (set of items) of questions of interest
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return plot generated with ggplot
  
  #get information about the items
  source("R/config_plot.R")
  
  #load information about labels of the items
  keep_var_wave <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen2_201124.csv"),
                                     delim = ";",
                                     col_names = TRUE, trim_ws = TRUE,
                                     locale = readr::locale(encoding = "latin1")) %>% 
    dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(questions = VAR_CONTENT, items = QUESTION)
  
  #prepare data
  d <- data %>%  
    dplyr::filter(stringr::str_detect(.$questions, pattern = question)) %>% 
    dplyr::filter(stringr::str_detect(.$PR, pattern = pr)) %>% 
    # NA's are not beeing droped
    tidyr::drop_na(answer) %>% 
    #group questions and answers
    dplyr::group_by(questions, answer) %>% 
    dplyr::summarise(tot = n()) %>% 
    dplyr::mutate(pct = tot / sum (tot)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(., keep_var_wave[, c("questions", "LABEL")], by = c("questions")) %>% 
    #remove all upto : https://stackoverflow.com/questions/12297859/remove-all-text-before-colon  
    dplyr::mutate(label = stringr::str_remove(LABEL, pattern = ".*:")) 
  
  # text
  txt <- d %>%
    group_by(questions) %>%
    summarise(tot_overall = sum(tot))
  
  # # merge data back
  # d <- d %>%
  #   left_join(., txt, by = "questions") %>%
  #   mutate(xlab_ = paste0("(n = ", tot_overall, ")"),
  #          xlab = paste(label, xlab_, sep = "\n"))
  
  
  #plot the results
  # str_wrap, wraps the strings with a specific width:
  p <- ggplot2::ggplot(d, ggplot2::aes(x = stringr::str_wrap(label, wrap), y = pct, fill = factor(answer))) +
    ggplot2::geom_bar(stat = "identity", 
                      position = ggplot2::position_stack(), width = .6) +
    ggplot2::scale_fill_manual(values = pal, 
                               breaks = attributes(pal)$names, labels = lab) + 
    scale_y_origin(labels = scales::percent) +
    ggplot2::guides(fill = guide_legend("", reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(pct < .02, "", 
                                                   scales::percent(pct, accuracy = 1))), 
                       position = ggplot2::position_stack(vjust = .5), size = 22*converter,
                       family = ggplot2::theme_get()$text$family) +
    ggplot2::labs(y = ylab, x = "", subtitle = paste0(title, "(n = ", txt$tot_overall[1], ").")) +
    ggplot2::coord_flip() +
    mytheme +
    ggplot2::theme(legend.position = "bottom")
  
  
  
  if(save == TRUE){
    ggplot2::ggsave(filename =  paste0(survey_plot_pr, stringr::str_sub(question,
                                                                        start = -5),
                                       "_", 
                                       pr,
                                       "_", 
                                       format(Sys.Date(), "%Y_%m_%d"),
                                       ".pdf"),
                    p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = cairo_pdf)
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "PDF"," has been saved")
    
    ggplot2::ggsave(filename = paste0(survey_plot_pr, stringr::str_sub(question,
                                                                       start = -5),
                                      "_", 
                                      pr,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "png"," has been saved")
  }else{
    print(p)
  }
  
  
}





#plot over both time points
plot_item_block_both_pr <- function(data, 
                                    item,
                                    title = NULL,
                                    question = NULL,
                                    wrap = NULL,
                                    save = FALSE, width = NULL, height = NULL){
  
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param item list of an item of the question of interest
  #' @param question defines the title of the question many rows you want in the plot (default = NULL)
  #' @param wrap defines the when (which position) a sting need to be wraped
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return plot generated with ggplot
  
  #get information about the items
  source("R/config_plot.R")
  
  #load information about labels of the items
  keep_var_wave <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen2_201124.csv"),
                                     delim = ";",
                                     col_names = TRUE, trim_ws = TRUE,
                                     locale = readr::locale(encoding = "latin1")) %>% 
    dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(questions = VAR_CONTENT, items = QUESTION)
  
  
  #prepare data
  d <- data %>%
    dplyr::filter(stringr::str_detect(.$questions, pattern = item)) %>%
    # filter(str_detect(.$PR, pattern = pr)) %>% 
    # NA's are beeing droped: needs to be discussed
    tidyr::drop_na(answer, PR) %>% #one answer to the PR is missing
    # mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>%
    dplyr::group_by(PR, time, questions, answer) %>%
    dplyr::summarise(tot = n()) %>%
    dplyr::mutate(pct = tot / sum (tot)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(PR = if_else(PR == "PR1", "PR1 (K)", PR)) #change one PR
  
  #add overall both time points
  df_o <- d %>% 
    dplyr::group_by(PR, questions, answer) %>% 
    dplyr::summarise(tot = sum(tot)) %>% 
    dplyr::mutate(pct = tot / sum(tot)) %>% 
    dplyr::mutate(time = "Alle") %>% 
    dplyr::bind_rows(d, .) %>%  
    dplyr::left_join(., keep_var_wave[, c("questions", "LABEL")], by = c("questions")) %>%
    #remove all upto : https://stackoverflow.com/questions/12297859/remove-all-text-before-colon
    dplyr::mutate(label_ = stringr::str_remove(LABEL, pattern = ".*:"),
                  label = stringr::str_replace_all(label_, "[[:punct:]]", " "))
  
  # add text
  df_t <- df_o %>%
    dplyr::group_by(PR, time, questions) %>%
    dplyr::summarise(tot_overall = sum(tot)) %>%
    dplyr::ungroup() %>% 
    dplyr::left_join(df_o, ., by = c("questions", "time", "PR")) %>% 
    # mutate(time_ = if_else(time == "T0", "erste Befragung", "zweite Befragung")) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot_overall,")"),
                  # xlab_1 = paste(time, xlab_, sep = "\n")) #,
                  xlab_2 = paste(time, xlab_, sep = " "))
  
  
  #add mean and SD per item
  dt_m <- df_t %>% 
    #get the overall per item
    dplyr::mutate(sum_calc = tot * as.numeric(answer)) %>% 
    #sum per questions and take the mean
    dplyr::group_by(PR, time, questions, tot_overall) %>% 
    dplyr::summarise(sum_id = sum(sum_calc)) %>% 
    dplyr::ungroup() %>% 
    #double check that again in the wide data format
    dplyr::mutate(mean_id = sum_id / tot_overall) %>% 
    #sd seems not to be wright somehow the sum gives another results...
    # sd_item = sqrt(SUM?(sum_id - mean(sum_id))^2) / (tot_overall - 1))
    #why is it duplicating everything 5 times?
    dplyr::left_join(., df_t[, c("xlab_2", "time", "PR")]) %>% 
    dplyr::filter(!duplicated(.)) %>% 
    dplyr::mutate(pct = 1) # adding pct for plotting the means
  
  #plot the results
  #str_wrap, wraps the strings with a specific width:
  #pay attention to the order of geom_text, took me a while to get it right: https://github.com/tidyverse/ggplot2/issues/3612
  p <- ggplot2::ggplot(data = df_t, ggplot2::aes(x = forcats::fct_rev(factor(xlab_2)),
                                                 y = pct)) +
    ggplot2::geom_bar(data = df_t, ggplot2::aes(fill = forcats::fct_rev(factor(answer))), 
                      stat = "identity", position = ggplot2::position_stack(reverse = TRUE), 
                      width = .89) +
    ggplot2::coord_flip()  +
    ggplot2::geom_text(data = df_t, 
                       ggplot2::aes(group = answer, label = if_else(pct < .02, "", 
                                                                    scales::percent(pct, accuracy = 1))), 
                       position = ggplot2::position_stack(vjust = .5), size = 22*converter,
                       family = ggplot2::theme_get()$text$family) + #size is not adjusting correctly
    ggplot2::geom_text(data = dt_m, ggplot2::aes(label = round(mean_id, 1)),
                       position = ggplot2::position_stack(vjust = 1.02),
                       size = 22*converter, family = ggplot2::theme_get()$text$family)  +
    
    #switch to y: https://stackoverflow.com/questions/52554822/save-a-ggplot2-time-series-plot-grob-generated-by-ggplotgrob
    ggplot2::facet_wrap(~ PR,  scales = "free_y", ncol = 1, strip.position = "left", drop = TRUE) +
    ggplot2::scale_fill_manual(values = pal, breaks = attributes(pal)$name, 
                               labels = lab) + 
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)), 
                                labels = scales::percent) +
    ggplot2::labs(y = "", x = "",
                  subtitle = stringr::str_wrap(paste0(title, unique(df_t$label_)),
                                               width = wrap)) +
    ggplot2::guides(fill = guide_legend("", reverse = TRUE)) + 
    mytheme_facet +
    ggplot2::theme(legend.position = "bottom")
  
  
  
  if(save == TRUE){
    ggplot2::ggsave(filename =  paste0(survey_plot_pr, item,
                                       "_", 
                                       format(Sys.Date(), "%Y_%m_%d"),
                                       ".pdf"),
                    p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = cairo_pdf)
    
    message("plot ", paste0("plots/", item, "_", format(Sys.Date(), "%Y_%m_%d")),
            "PDF"," has been saved")
    
    ggplot2::ggsave(filename = paste0(survey_plot_pr, item,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", paste0("plots/", item, "_", format(Sys.Date(), "%Y_%m_%d")),
            "png"," has been saved")
  }else{
    print(p)
  }
  
}



message("functions: \n
        - plot_item_t1_pr
        - plot_item_block_both_pr
        \n are ready")
