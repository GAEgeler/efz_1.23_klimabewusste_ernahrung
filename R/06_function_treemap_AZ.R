# fucntion to plot grafics for survey data asz 

#status december 2020

#load packages
library(dplyr)
library(ggplot2)
library(treemapify)

plot_item_treemap <- function(data, wave = NULL, question, asz = NULL, 
                             width = NULL, height = NULL, save = FALSE){
  
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (t0, t1 or both)
  #' @param question string containing which question of interest
  #' @param width defines witdh of the plot
  #' @param height defines height of the plot
  #' @return plot with ggplot
  
  # load mytheme and fonts
  source("R/config_plot.R")
  
  # load information about the items
  source("R/06_function_read_items_survey_AZ.R")
  
  #prepare data 
  d <- data %>%
    # first filter according parameters from above
    filter(time == toString(wave)) %>% 
    filter(questions == toString(question)) %>% 
    filter(str_detect(.$ASZ, pattern = asz)) %>% 
    mutate(answer = if_else(is.na(.$answer), "nicht beantwortet", .$answer)) %>% 
    group_by(questions, answer) %>% 
    summarise(tot = n()) %>% 
    mutate(pct = tot / sum (tot)) %>% 
    ungroup() %>% 
    left_join(., items, by= "questions")
  
  # add text for xlab
  df_t <- d %>% 
    group_by(questions, items) %>% 
    summarise(tot_asz = sum(tot)) %>% 
    mutate(xlab_ = paste0("(n = ", tot_asz, ")"),
           # xlab_2 = paste(ASZ, time, sep = " "),
           xlab = paste(items, xlab_, sep = "\n")) %>%  
    left_join(., d, by = c("questions", "items"))
  
  
  p <- ggplot2::ggplot(df_t, aes(area = pct, fill = answer,
                        label = scales::percent(pct, accuracy = 1))) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(fontface = "italic", colour = "black", place = "centre",
                      grow = FALSE, min.size = 18) +
    scale_fill_manual(values = pal2) + 
    guides(fill = guide_legend(unique(df_t$xlab))) +
    labs(subtitle = asz) +
    mytheme 
  
  #save plot
  if(save == TRUE){
    ggplot2::ggsave(filename = paste0(survey_plot_asz, question, "_", wave, "_", 
                             format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
           plot = p,
           width = width,
           height = height,
           device = cairo_pdf)
    
    ggplot2::ggsave(filename = paste0(survey_plot_asz, question, "_", wave, "_", 
                             format(Sys.Date(), "%Y_%m_%d"), ".png"),
           plot = p,
           width = width,
           height = height,
           device = "png")
    
    message("plot ", paste0(survey_plot_asz, question, "_", format(Sys.Date(), "%Y_%m_%d")),
            "PDF & png"," has been saved!")
  }else{
    print(p)
  }
  
  
}

message("function plot_item_treemap is ready")
