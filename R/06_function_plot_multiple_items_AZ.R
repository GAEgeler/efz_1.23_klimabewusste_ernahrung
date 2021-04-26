# fucntion to plot grafics for survey data asz 


plot_item_block_all_asz <- function(data, wave = NULL, 
                            question,
                            asz = NULL, 
                            save = NULL, width = NULL, height = NULL){
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (both, NULL means per canteen)
  #' @param question list of a test batterie of questions of interest
  #' @param width defines witdh of the plot
  #' @param height defines height of the plot
  #' @description creates a plot overall ASZ spitl according the time points 
  #' @return plot with ggplot
  #' @export
  
  #get information about the items
  source("R/06_function_read_items_survey_AZ.R")
  source("R/config_plot.R")
  
  #prepare data
  # 
    d <- data %>% 
      filter(stringr::str_detect(.$questions, pattern = question)) %>% 
      # NA's are not beeing droped
      mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
      group_by(questions, time, answer) %>% 
      summarise(tot = n()) %>% 
      mutate(pct = tot / sum (tot)) %>% 
      ungroup() %>% 
      left_join(., items, by= "questions") 
    
    #add tet
    df_t <- d %>% 
      group_by(questions, time) %>% 
      summarise(tot_overall = sum(tot)) %>% 
      ungroup() %>% 
      mutate(xlab_ = paste0("(n = ", tot_overall, ")"),
             xlab = paste(time, xlab_, sep = " ")) %>%
            #xlab = paste(if_else(time == "T0", "erste Befragung", "zweite Befragung"), xlab_, sep = "\n")
      # merge data back
     left_join(., d, by = c("questions", "time")) 
    
    #plot thus grid plot
    p <- ggplot2::ggplot(df_t, ggplot2::aes(x = forcats::fct_rev(factor(xlab)), y = pct, fill = factor(answer, levels = pattrn))) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(reverse = TRUE), width = .6) +
      ggplot2::scale_fill_manual(values = pal) + 
      scale_y_origin(labels = scales::percent) +
      ggplot2::guides(fill = ggplot2::guide_legend("", reverse = FALSE)) +
      ggplot2::geom_text(ggplot2::aes(label = scales::percent(pct, accuracy = 1)), 
                position = ggplot2::position_stack(reverse = TRUE, vjust = .5),
                size = 22*converter, family = ggplot2::theme_get()$text$family) +
      ggplot2::labs(y = "", x = "") +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ items,  scales = "free_y", ncol = 1, strip.position = "top", drop = TRUE) +
      mytheme +
      ggplot2::theme(legend.position = "bottom")
    
    if(save == TRUE){
      ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                        stringr::str_sub(question,start = -5),
                                        "_", 
                                        wave,
                                        asz,
                                        "_", 
                                        format(Sys.Date(), "%Y_%m_%d"),
                                        ".pdf"),
                      p,
                      width = width,
                      height = height,
                      dpi = 300,
                      device = cairo_pdf)
      
      message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
              "PDF", " has been saved")
      
      ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                        stringr::str_sub(question,start = -5),
                                        "_", 
                                        wave,
                                        asz,
                                        "_", 
                                        format(Sys.Date(), "%Y_%m_%d"),
                                        ".png"),
                      p,
                      width = width,
                      height = height,
                      dpi = 300,
                      device = "png")
      
      message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
              "png", " has been saved")
      
    }else{
      print(p)
    }
    
    
}


plot_item_block_one_asz <- function(data, wave = NULL, 
                                       question,
                                       asz = NULL, 
                                       lab = NULL,
                                       wrap = NULL,
                                       save = NULL, width = NULL, height = NULL){
    
    #' @param data data frame (e.g. tibble)
    #' @param wave string containing which wave (both, NULL means per canteen)
    #' @param question list of a test batterie of questions of interest
    #' @param lab string containg the subtilte of the plot
    #' @param width defines witdh of the plot
    #' @param height defines height of the plot
    #' @description creates a plot for one ASZ an ONE time point
    #' @return plot with ggplot
    #' @export
    
    #get information about the items
    source("R/06_function_read_items_survey_AZ.R")
    source("R/config_plot.R")  
  
    # #when ploting only over one point of time & specific asz
    d <- data %>%
      filter(time == toString(wave) & ASZ == toString(asz)) %>%
      filter(stringr::str_detect(.$questions, pattern = question)) %>%
      # NA's are not beeing droped
      mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>%
      group_by(questions, answer) %>%
      summarise(tot = n()) %>%
      mutate(pct = tot / sum (tot)) %>%
      ungroup() %>%
      left_join(., items, by= "questions")

    # text
    dt_t <- d %>%
      group_by(questions) %>%
      summarise(tot_overall = sum(tot)) %>% 
      left_join(., d, by = "questions") %>%
      mutate(xlab_ = paste0("(n =", tot_overall,")"),
             xlab = paste(items, xlab_, sep = " "))

    #plot the results
    # str_wrap, wraps the strings with a specific width:
    p <- ggplot2::ggplot(dt_t, aes(x = "", y = pct, 
                                fill = factor(answer, levels = pattrn))) +
      geom_bar(stat = "identity", position = position_stack(reverse = T), width = .6) +
      scale_fill_manual(values = pal) +
      scale_x_discrete(labels = NULL, breaks = NULL) +
      scale_y_origin(labels = scales::percent) +
      guides(fill = guide_legend("")) +
      geom_text(aes(label = scales::percent(pct, accuracy = 1)),
                position = position_stack(reverse = T, vjust = .5),
                size = 22*converter, family = theme_get()$text$family) +
      labs(y = "", x = "", subtitle = lab) +
      coord_flip() +
      facet_wrap( ~ xlab, scales = "free_y", ncol = 1) +
      mytheme +
      theme(legend.position = "bottom") 
            # axis.title.x=element_blank(),
            # axis.text.x=element_blank(),
            # axis.ticks.x=element_blank())
    
    
    if(save == TRUE){
      ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                        stringr::str_sub(question,start = -5),
                                        "_", 
                                        wave,
                                        asz,
                                        "_", 
                                        format(Sys.Date(), "%Y_%m_%d"),
                                        ".pdf"),
                      p,
                      width = width,
                      height = height,
                      dpi = 300,
                      device = cairo_pdf)
      
      message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
              "PDF", " has been saved")
      
      ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                        stringr::str_sub(question,start = -5),
                                        "_", 
                                        wave,
                                        asz,
                                        "_", 
                                        format(Sys.Date(), "%Y_%m_%d"),
                                        ".png"),
                      p,
                      width = width,
                      height = height,
                      dpi = 300,
                      device = "png")
      
      message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
              "png", " has been saved")
      
    }else{
      print(p)
    }
    
    
}


plot_item_block_two_asz <- function(data, wave = NULL, 
                                    question,
                                    asz = NULL, 
                                    save = NULL, width = NULL, height = NULL){
  
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (both, NULL means per canteen)
  #' @param question list of a test batterie of questions of interest
  #' @param width defines witdh of the plot
  #' @param height defines height of the plot
  #' @description creates a plot for one ASZ for BOTH time point
  #' @return plot with ggplot
  #' @export
    
  
  #when plotting over both times but for a specific asz
  d <- data %>% 
      filter(str_detect(.$questions, pattern = question)) %>% 
      filter(str_detect(.$ASZ, pattern = asz)) %>% 
      # NA's are not beeing droped
      mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
      group_by(questions, time, answer) %>% 
      summarise(tot = n()) %>% 
      mutate(pct = tot / sum (tot)) %>% 
      ungroup() %>% 
      left_join(., items, by= "questions")
    
    # text
    df_t <- d %>% 
      group_by(questions, time) %>% 
      summarise(tot_overall = sum(tot)) %>% 
      ungroup() %>% 
      mutate(xlab_ = paste0("(n = ", tot_overall, ")"),
             xlab = paste(time, xlab_, sep = " ")) %>%
             # xlab = paste(if_else(time == "T0", "erste Befragung", "zweite Befragung"), xlab_, sep = "\n")) %>% 
      # merge data back
      left_join(., d, by = c("questions", "time")) 
    
    #plot the results
    # str_wrap, wraps the strings with a specific width:
    p <- ggplot2::ggplot(df_t, ggplot2::aes(x = forcats::fct_rev(factor(xlab)), y = pct, fill = factor(answer, levels = pattrn))) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(reverse = TRUE), width = .6) +
      ggplot2::scale_fill_manual(values = pal) + 
      scale_y_origin(labels = scales::percent) +
      ggplot2::guides(fill = ggplot2::guide_legend("", reverse = FALSE)) +
      ggplot2::geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
                position = ggplot2::position_stack(reverse = TRUE, vjust = .5), 
                size = 22*converter, family = ggplot2::theme_get()$text$family) +
      ggplot2::labs(y = "", x = "") +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ items,  scales = "free_y", ncol = 1, strip.position = "top", drop = TRUE) +
      mytheme +
      ggplot2::theme(legend.position = "bottom")
    
    message("plot data of canteen ", asz, " is beeing processed")

  
  if(save == TRUE){
    ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                      stringr::str_sub(question,start = -5),
                                                 "_", 
                                                 wave,
                                                 asz,
                                                 "_", 
                                                 format(Sys.Date(), "%Y_%m_%d"),
                                                 ".pdf"),
           p,
           width = width,
           height = height,
           dpi = 300,
           device = cairo_pdf)
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "PDF", " has been saved")
    
    ggplot2::ggsave(filename = paste0(survey_plot_asz, 
                                      stringr::str_sub(question,start = -5),
                                      "_", 
                                      wave,
                                      asz,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", paste0("plots/", question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "png", " has been saved")
    
  }else{
    print(p)
  }
  
  
}

message("functions: \n
        -plot_item_block_all_asz
        -plot_item_block_one_asz
        -plot_item_block_two_asz
        
        \nare ready")
