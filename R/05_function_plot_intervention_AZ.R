# function to plot pre post intervention per canteen ASZ 

#state: january 2021

#required libraries
library(magrittr)
library(dplyr)

plot_interv_asz <- function(data, asz = NULL, 
                            save = FALSE,
                            lab = NULL,
                            height = NULL, 
                            width = NULL){
  #' @author gian-andrea egeler
  #' @param data data set (as data frame or tibble)
  #' @param asz defines the asz
  #' @param lab defines the title of the plot
  #' @param save defines if the grafic needs to be saved or not
  #' @return plot
  #' @export
  
  # prepare data
  if(asz != toString("all")){
    #only single canteen
    df <- data %>% 
      mutate(kw = lubridate::isoweek(date)) %>% 
      filter(ASZ == toString(asz)) %>% 
      #wochenhit in or out? => i quess there is some bias if not included
      group_by(kw, condit, meal_label) %>% 
      summarise(tot = sum(tot_sold)) %>%
      mutate(pct = tot / sum(tot),
             ASZ = toString(asz)) %>% 
      #change names of the canteens resp. ASZ
      mutate(ASZ_p = case_when(ASZ == "AZ1" ~ "AZ1 (K)",
                               TRUE ~ ASZ))
    
    #prepare xlab
    df_p <- df %>% 
      group_by(kw, condit) %>% 
      summarise(tot_t = sum(tot)) %>% 
      ungroup() %>% 
      #change kw for plotting
      mutate(wks = case_when(kw == 36 ~ "Wo1",
                             kw == 37 ~ "Wo2",
                             kw == 38 ~ "WoInt3",
                             kw == 39 ~ "WoInt4",
                             kw == 40 ~ "WoInt5", 
                             kw == 41 ~ "WoInt6",
                             kw == 42 ~ "WoInt7",
                             kw == 43 ~ "WoInt8",
                             kw == 44 ~ "Wo9")) %>% 
      mutate(xlab_ = paste0("(n = ", tot_t, ")"),
             xlab = paste(wks, xlab_, sep = "\n")) %>% 
      #add color for plots
      mutate(wks_col = paste("<span style = 'color: ",
                             if_else(str_detect(wks, "WoInt"), "grey50", "black"),
                             ";'>", 
                             xlab,
                             "</span>", sep = "")) %>% 
      left_join(., df, by = c("condit", "kw"))  
    
    #plot: to get the right order, the xlab needs to be set as factor again, however the order stays the same as defined above!
    #check out: https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors
    p <- ggplot2::ggplot(data = df_p, ggplot2::aes(x = as_factor(xlab), y = pct, 
                                                   fill = factor(meal_label, labels = c("Fleisch", "Vegetarisch")))) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .5) + 
      ggplot2::geom_rect(ggplot2::aes(xmin = 3-.25, xmax = 8+.25, ymin = 0, ymax = 1), fill = "grey70", alpha = .05) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .5) + 
      ggplot2::scale_fill_manual(values = c("Fleisch" = "#eeda51", "Vegetarisch" = "#c1cf3b")) +
      ggplot2::geom_text(aes(label=ifelse(pct > .015,
                                          scales::percent(round(pct, 2),
                                                          accuracy = 1), "")), 
                         size = 22 * converter, position = ggplot2::position_stack(vjust = 0.52),
                         family = ggplot2::theme_get()$text$family)+
      ggplot2::labs(x = "", y = "\nVerkaufte Gerichte in Prozent\n", subtitle = lab)+
      ggplot2::guides(fill = ggplot2::guide_legend("Menüwahl")) +
      scale_y_origin(labels = scales::percent) +
      mytheme # +
    # theme(axis.text.x = element_markdown())
    
  }else{
    #overall 
    df_t <- data %>%
      group_by(ASZ, condit, meal_label) %>%
      summarise(tot = sum(tot_sold)) %>%
      mutate(pct = tot / sum(tot)) %>%
      ungroup()  %>%
      #change names of the canteens resp. ASZ
      mutate(ASZ_p = case_when(ASZ == "AZ1" ~ "AZ1 (K)",
                               TRUE ~ ASZ))
    
    #prepare xlab
    df_p <- df_t %>% 
      mutate(condit = forcats::as_factor(condit)) %>% 
      mutate(condit = forcats::fct_relevel(condit, "Vorher")) %>% # put vorher as first factor
      group_by(condit, ASZ_p) %>% 
      summarise(tot_t = sum(tot)) %>% 
      ungroup() %>% 
      mutate(xlab_ = paste("(n = ", tot_t,")", sep = ""),
             xlab = paste(ASZ_p, xlab_, sep = "\n")) %>% 
      left_join(., df_t, by = c("condit", "ASZ_p")) 
    
    
    #plot: to get the right order, the xlab needs to be set as factor again, however the order stays the same as defined above!
    p <- ggplot(df_p, aes(x = xlab, y = pct, 
                          fill = factor(meal_label, labels = c("Fleisch", "Vegetarisch")))) +
      geom_bar(stat = "identity", position = position_stack(), width = .6) + 
      facet_grid(~ as_factor(condit), scales = "free")+
      scale_fill_manual(values = c("Fleisch" = "#eeda51", "Vegetarisch" = "#c1cf3b")) +
      geom_text(aes(label=ifelse(pct > .015,
                                 scales::percent(round(pct, 2),
                                                 accuracy = 1), "")), 
                size = 22*converter, position = position_stack(vjust = 0.52),
                family = theme_get()$text$family)+
      labs(x = "", y = "\nVerkaufte Gerichte in Prozent\n")+
      guides(fill = guide_legend("Menüwahl"))+
      scale_y_origin(labels=scales::percent)+
      mytheme 
    
  }
  
  
  if(save == TRUE){
    ggplot2::ggsave(plot = p,
                    filename =  paste0(till_plot_asz, asz, "_", 
                                       format(Sys.Date(), "%Y%m%d"),
                                       ".pdf"),
                    width = width,
                    height = height,
                    device = cairo_pdf)
    
    message("plot ", paste0(asz,
                            "_", 
                            format(Sys.Date(), "%Y_%m_%d"),
                            ".pdf"),
            " has been saved")
    
    ggplot2::ggsave(plot = p,
                    filename =  paste0(till_plot_asz, asz, "_", 
                                       format(Sys.Date(), "%Y%m%d"),
                                       ".png"),
                    width = width,
                    height = height,
                    device = "png")
    
    message("plot ", paste0(asz,
                            "_", 
                            format(Sys.Date(), "%Y_%m_%d"),
                            ".png"),
            " has been saved")
    
  }else{
    print(p)
  }
  
  
}

message("function plot_interv_asz is ready")
