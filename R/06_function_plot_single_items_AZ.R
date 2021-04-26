# fucntion to plot grafics for survey data asz 

#status december 2020

#load packages
library(dplyr)
library(ggplot2)

plot_item_single <- function(data, wave = NULL, tit_guide = "", question, asz = NULL, 
                             width = NULL, height = NULL, save = FALSE){
  
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (t0, t1 or both)
  #' @param tit_guide string containing the title of the legend
  #' @param question string containing which question of interest
  #' @param width defines witdh of the plot
  #' @param height defines height of the plot
  #' @return plot with ggplot
  #' @export
  
  # load mytheme and fonts
  source("R/config_plot.R")
  
  # load information about the items
  source("R/06_function_read_items_survey_AZ.R")
  
  #prepare data
  if(!is.character(asz) & wave == "both"){
    #plot figure if no asz is defined
    d <- data %>% 
    filter(questions == toString(question)) %>% 
    group_by(ASZ, time, answer) %>% 
    summarise(tot = n()) %>% 
    mutate(pct = tot / sum(tot)) %>%
    ungroup() %>% 
    #change na's to keine angaben
    mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
    # change names of ASZ for plot
    mutate(ASZ = case_when(ASZ == "AZ1" ~ "AZ1 (K)",
                           TRUE  ~ ASZ))
    
    # add text for xlab
    df_t <- d %>% 
      group_by(ASZ, time) %>% 
      summarise(tot_asz = sum(tot)) %>% 
      mutate(xlab_ = paste0("(n = ", tot_asz, ")"),
             xlab = paste(time, xlab_, sep = "\n")) %>% 
             # xlab = paste(if_else(time == "T0", "erste Befragung", "zweite Befragung"), xlab_, sep = "\n")) %>%  
      left_join(., d, by = c("ASZ", "time"))
    
    
    p <- ggplot(df_t, aes(x = xlab, y = pct, fill = factor(answer, levels = pattrn))) +
      geom_bar(stat = "identity", position = position_stack(reverse = T), width = .6) +
      scale_fill_manual(values = pal) + 
      scale_y_origin(labels = scales::percent) +
      guides(fill = guide_legend(tit_guide, reverse = T)) +
      geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
                position = position_stack(reverse = T, vjust = .5), 
                size = 22*converter, family = theme_get()$text$family) +
      labs(y = "Antworten in Prozent", x = "") +
      facet_grid(. ~ ASZ, scales = "free") +
      mytheme 
    
  
  }else if(!is.character(asz)){
    #plot figure if no asz is defined
    #attention here is a new_pattern required
    d <- data %>% 
      filter(time == toString(wave) & questions == toString(question)) %>% 
      group_by(ASZ, answer) %>% 
      summarise(tot = n()) %>% 
      mutate(pct = tot / sum(tot)) %>%
      ungroup() %>% 
      #change na's to keine angaben
      mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
      # change names of ASZ for plot
      mutate(ASZ = case_when(ASZ == "AZ1" ~ "AZ1 (K)"))
    
    # add text for xlab
    txt <- d %>% 
      group_by(ASZ) %>% 
      summarise(tot_asz = sum(tot)) %>% 
      mutate(xlab_ = paste0("(", tot_asz, ")"),
             xlab = paste(ASZ, xlab_, sep = "\n"))
    
    
    #merge data back
    d <- d %>% 
      left_join(., txt, by = "ASZ")
    
    
    p <- ggplot(d, aes(x = xlab, y = pct, fill = factor(answer, levels = pattrn, labels = new_labels))) +
      geom_bar(stat = "identity", position = position_stack(reverse = T), width = .6) +
      scale_fill_manual(values = pal) + 
      scale_y_origin(labels = scales::percent) +
      guides(fill = guide_legend("")) +
      geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
                position = position_stack(reverse = T, vjust = .5), 
                size = 22*converter, family = theme_get()$text$family) +
      labs(y = "", x = "") +
      coord_flip() +
      mytheme +
      theme(legend.position = "bottom") 
    
  }else{
    #when asz is defiend
    
    d <- data %>% 
      filter(time == toString(wave)) %>% 
      filter(stringr::str_detect(.$questions, pattern = question)) %>% 
      filter(stringr::str_detect(.$ASZ, pattern = asz)) %>% 
      group_by(questions, answer) %>% 
      summarise(tot = n()) %>% 
      mutate(pct = tot / sum(tot)) %>%
      ungroup() %>% 
      #change na's to keine angaben
      mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
      left_join(., items, by= "questions")
    
    # add text for xlab
    txt <- d %>% 
      group_by(items) %>% 
      summarise(tot_asz = sum(tot)) %>% 
      mutate(xlab_ = paste0("(", tot_asz, ")"),
             xlab = paste(items, xlab_, sep = "\n"))
    
    #merge data back
    d <- d %>% 
      left_join(., txt, by = "items")
    
    
    p <- ggplot(d, aes(x = xlab, y = pct, fill = factor(answer, levels = pattrn2))) +
      geom_bar(stat = "identity", position = position_stack(reverse = F), width = .6) +
      scale_fill_manual(values = pal2) + 
      scale_y_origin(labels = scales::percent) +
      guides(fill = guide_legend("")) +
      geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
                position = position_stack(reverse = F, vjust = .5), 
                size = 22*converter,
                family = theme_get()$text$family) +
      labs(y = "", x = "") +
      mytheme 
    
    
    }
  
   
  
 # save plot
  if(save == TRUE){
    ggsave(filename = paste0(survey_plot_asz, question, "_", wave, "_", 
                  format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
           plot = p,
           width = width,
           height = height,
           device = cairo_pdf)
    
    message("plot ", paste0(survey_plot_asz, question, "_", format(Sys.Date(), "%Y_%m_%d")),
                            "PDF"," has been saved!")
    
    ggsave(filename = paste0(survey_plot_asz, question, "_", wave, "_", 
                             format(Sys.Date(), "%Y_%m_%d"), ".png"),
           plot = p,
           width = width,
           height = height,
           device = "png")
    
    message("plot ", paste0(survey_plot_asz, question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "png"," has been saved!")
    
  }else{
    print(p)
  }
  
}

message("function plot_item_single is ready")
