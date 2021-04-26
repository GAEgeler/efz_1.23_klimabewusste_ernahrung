# function to plot intervention per canteen

# state: december 2020

#source mytheme information
source("R/config_plot.R")
source("R/config_path.R")


#plot intervention per canteen
plot_intervention_pr <- function(data, 
                                 canteen_pr = NULL,
                                 # menu_identical = NULL,
                                 save = FALSE, 
                                 width = NULL, 
                                 height = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param canteen string containing which canteen
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  
  
  #only single canteen
  df <- data %>% 
    dplyr::mutate(kw = lubridate::isoweek(date)) %>% 
    dplyr::filter(pr == toString(canteen_pr)) %>% 
    #filter only between kw34 and kw46
    dplyr::filter(kw >= 38 & kw <= 43) %>% 
    #wochenhit in or out? => i quess there is some bias if not included
    dplyr::group_by(kw, meal_label) %>% 
    dplyr::summarise(tot = sum(tot_sold)) %>%
    dplyr::mutate(pct = tot / sum(tot),
                  pr = toString(canteen_pr)) %>% 
    #change names of the canteens resp. ASZ
    dplyr::mutate(pr_p = dplyr::case_when(pr == "PR1" ~ "PR1 (K)",
                                          TRUE ~ pr))
  
  #prepare xlab
  df_p <- df %>% 
    dplyr::group_by(kw) %>% 
    dplyr::summarise(tot_t = sum(tot)) %>% 
    dplyr::ungroup() %>% 
    #change kw for plotting
    dplyr::mutate(wks = dplyr::case_when(kw == 38 ~ "WoInt7",
                                         kw == 39 ~ "WoInt8", 
                                         kw == 40 ~ "WoInt9",
                                         kw == 41 ~ "WoInt10",
                                         kw == 42 ~ "WoInt11",
                                         kw == 43 ~ "WoInt12")) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot_t, ")"),
                  xlab = paste(wks, xlab_, sep = "\n")) %>% 
    dplyr::left_join(., df, by = c("kw")) 
  
  
  #plot
  # order factor according condit is not working
  p <- ggplot2::ggplot(df_p, ggplot2::aes(x = forcats::as_factor(xlab), y = pct, 
                                          fill = factor(meal_label, levels = c("buffet",  "fish",
                                                                               "meat", "vegetarian"),
                                                        labels = c("Buffet", "Fisch", "Fleisch", "Vegetarisch")))) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .6) +
    ggplot2::scale_fill_manual(values = pal) +
    scale_y_origin(labels = scales::percent) +
    ggplot2::guides(fill = ggplot2::guide_legend("Menüinhalt")) +
    ggplot2::geom_text(ggplot2::aes(label = dplyr::if_else(pct > 0.02, scales::percent(pct, accuracy = 1), "")), 
                       position = ggplot2::position_stack(reverse = F, vjust = .5), size = 22*converter,
                       family = ggplot2::theme_get()$text$family) +
    ggplot2::labs(y = "Menüverkäufe in Prozent", x = "") +
    mytheme
  
  
  if(save == TRUE){
    ggplot2::ggsave(filename = paste0(till_plot_pr, "intervention_",
                                      canteen_pr,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".pdf"),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = cairo_pdf)
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".pdf",
            " has been saved")
    
    ggplot2::ggsave(filename = paste0(till_plot_pr, "intervention_",
                                      canteen_pr,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png")
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".png",
            " has been saved")
    
  }else{
    print(p)
  }
}


#plot intervention overall
plot_overall_pr <- function(data, 
                            save = FALSE, 
                            width = NULL, 
                            height = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  #' @export
  
  #prepare data
  df_t <- data %>%
    #change kw for plotting
    dplyr::mutate(condit = dplyr::case_when(kw == 32 ~ "Vorher",
                                            kw == 33 ~ "Vorher",
                                            kw == 34 ~ "Vorher",
                                            kw == 35 ~ "Vorher",
                                            kw == 36 ~ "Vorher",
                                            kw == 37 ~ "Vorher",
                                            kw == 38 ~ "Intervention",
                                            kw == 39 ~ "Intervention", 
                                            kw == 40 ~ "Intervention",
                                            kw == 41 ~ "Intervention",
                                            kw == 42 ~ "Intervention",
                                            kw == 43 ~ "Intervention",
                                            kw == 44 ~ "Nachher",
                                            kw == 45 ~ "Nachher")) %>% 
    dplyr::group_by(pr, condit, meal_label) %>%
    dplyr::summarise(tot = sum(tot_sold)) %>%
    dplyr::mutate(pct = tot / sum(tot)) %>%
    dplyr::ungroup() %>% 
    #drop ke 32 and 46
    tidyr::drop_na(condit)
  
  #prepare xlab
  df_p <- df_t %>% 
    dplyr::mutate(condit = forcats::as_factor(condit)) %>% 
    dplyr::mutate(condit = forcats::fct_relevel(condit, "Vorher")) %>% # put vorher as first factor
    dplyr::group_by(pr, condit) %>% 
    dplyr::summarise(tot_t = sum(tot)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(xlab_ = paste("(n = ", tot_t,")", sep = ""),
                  xlab = paste(condit, xlab_, sep = "\n")) %>% 
    dplyr::left_join(., df_t, by = c("condit", "pr")) %>%  # rearrange factor for plots
    dplyr::mutate(meal_label = factor(meal_label, levels = c("buffet", "fish", "meat", "vegetarian"),
                                      labels = c("Buffet", "Fisch", "Fleisch", "Vegetarisch")))
  
  
  #plot: to get the right order, the xlab needs to be set as factor again, however the order stays the same as defined above!
  p <- ggplot2::ggplot(df_p, ggplot2::aes(x = forcats::as_factor(xlab), y = pct, 
                                          fill = meal_label)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .6) + 
    ggplot2::facet_grid(~ pr, scales = "free")+
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(pct > .015,
                                                   scales::percent(round(pct, 2),
                                                                   accuracy = 1), "")), 
                       size = 22*converter, position = ggplot2::position_stack(vjust = 0.52),
                       family = ggplot2::theme_get()$text$family)+
    ggplot2::labs(x = "", y = "\nVerkaufte Gerichte in Prozent\n")+
    ggplot2::guides(fill = ggplot2::guide_legend("Menüinhalt"))+
    scale_y_origin(labels = scales::percent)+
    mytheme 
  
  if(save == TRUE){
    ggplot2::ggsave(filename = paste0(till_plot_pr, "overall_agg_kw32kw45",
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".pdf"),
                    plot = p,
                    width = width,
                    height = height,
                    device = cairo_pdf, limitsize = FALSE)
    
    message("plot ", "overall",
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".pdf",
            " has been saved")
    
    ggplot2::ggsave(filename = paste0(till_plot_pr, "overall_agg_kw32kw45",
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png", limitsize = FALSE)
    
    message("plot ", "overall",
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".png",
            " has been saved")
    
  }else{
    print(p)
  }
  
}

#plot overall
plot_week_pr <- function(data, 
                         canteen_pr = NULL,
                         save = FALSE, 
                         width = NULL, 
                         height = NULL){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param pr defines the canteen of intrest
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  #' @export
  
  #only single canteen
  df <- data %>% 
    dplyr::mutate(kw = lubridate::isoweek(date)) %>% 
    dplyr::filter(pr == toString(canteen_pr)) %>% 
    #filter only between kw34 and kw46
    dplyr::filter(kw >= 32 & kw < 46) %>% 
    #wochenhit in or out? => i quess there is some bias if not included
    dplyr::group_by(kw, meal_label) %>% 
    dplyr::summarise(tot = sum(tot_sold)) %>%
    dplyr::mutate(pct = tot / sum(tot),
                  pr = toString(canteen_pr)) %>% 
    #change names of the canteens resp. ASZ
    dplyr::mutate(pr_p = dplyr::case_when(pr == "PR1" ~ "PR1 (K)",
                                          TRUE ~ pr))
  
  #prepare xlab
  df_p <- df %>% 
    dplyr::group_by(kw) %>% 
    dplyr::summarise(tot_t = sum(tot)) %>% 
    dplyr::ungroup() %>% 
    #change kw for plotting
    dplyr::mutate(wks = dplyr::case_when(kw == 32 ~ "Wo1",
                                         kw == 33 ~ "Wo2",
                                         kw == 34 ~ "Wo3",
                                         kw == 35 ~ "Wo4",
                                         kw == 36 ~ "Wo5",
                                         kw == 37 ~ "Wo6",
                                         kw == 38 ~ "WoInt7",
                                         kw == 39 ~ "WoInt8", 
                                         kw == 40 ~ "WoInt9",
                                         kw == 41 ~ "WoInt10",
                                         kw == 42 ~ "WoInt11",
                                         kw == 43 ~ "WoInt12",
                                         kw == 44 ~ "Wo13",
                                         kw == 45 ~ "Wo14")) %>% 
    dplyr::mutate(xlab_ = paste0("(n = ", tot_t, ")"),
                  xlab = paste(wks, xlab_, sep = "\n")) %>% 
    dplyr::left_join(., df, by = c("kw"))  
  
  
  #plot: to get the right order, the xlab needs to be set as factor again, however the order stays the same as defined above!
  p <- ggplot2::ggplot(df_p, ggplot2::aes(x = forcats::as_factor(xlab), y = pct, 
                                          fill = factor(meal_label, levels = c("buffet",  "fish",
                                                                               "meat", "vegetarian"),
                                                        labels = c("Buffet", "Fisch", "Fleisch", "Vegetarisch")))) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(),
                      width = .5) + 
    ggplot2::geom_rect(aes(xmin = 7-.25, xmax = 12+.25, ymin = 0, ymax = 1), fill = "grey80", alpha = .05) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(),
                      width = .5) +
    ggplot2::scale_fill_manual(values = c("Fleisch" = "#eeda51", "Fisch" = "#fcf18e",
                                          "Vegetarisch" = "#c1cf3b", 
                                          "Buffet" = "#f9ecbc")) +
    ggplot2::geom_text(ggplot2::aes(label = dplyr::if_else(pct > .015,
                                                           scales::percent(round(pct, 2),
                                                                           accuracy = 1), "")), 
                       size = 22 * converter, position = ggplot2::position_stack(vjust = 0.52),
                       family = ggplot2::theme_get()$text$family)+
    ggplot2::labs(x = "", y = "\nVerkaufte Gerichte in Prozent\n")+
    ggplot2::guides(fill = ggplot2::guide_legend("Menüinhalt")) +
    scale_y_origin(labels=scales::percent) +
    mytheme
  
  
  if(save == TRUE){
    ggplot2::ggsave(filename = paste0(till_plot_pr, "overall_kw32kw45_",
                                      canteen_pr,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".pdf"),
                    plot = p,
                    width = width,
                    height = height,
                    device = cairo_pdf, limitsize = FALSE)
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".pdf",
            " has been saved")
    
    ggplot2::ggsave(filename = paste0(till_plot_pr, "overall_kw32kw45_",
                                      canteen_pr,
                                      "_", 
                                      format(Sys.Date(), "%Y_%m_%d"),
                                      ".png"),
                    plot = p,
                    width = width,
                    height = height,
                    dpi = 300,
                    device = "png", limitsize = FALSE)
    
    message("plot ", canteen_pr,
            "_", 
            format(Sys.Date(), "%Y_%m_%d"),
            ".png",
            " has been saved")
    
  } else{
    print(p)
  }
}


message("functions:\n 
        - plot_pr
        - plot_overall_pr
        - plot_week_pr 
        \nare ready!")
