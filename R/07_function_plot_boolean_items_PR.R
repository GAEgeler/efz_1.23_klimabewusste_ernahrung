# fucntion to plot grafics for survey data PR (logical structure)


#state: december 2020

plot_item_boolean_pr <- function(data,
                                   question, 
                                   pr = NULL, 
                                   wrap = NULL,
                                   ylab = NULL,
                                   xlab = NULL,
                                   subtitle = NULL,
                                   width = NULL, 
                                   height = NULL, 
                                   save = FALSE){
  #' @author gian-andrea egeler
  #' @param data data frame (e.g. tibble)
  #' @param wave string containing which wave (t0 or t1)
  #' @param wrap numeric, contains width of string to wrap
  #' @param question string containing which question of interest, format = logical
  #' @param width defines width of the plot
  #' @param height defines height of the plot
  #' @return barplot with ggplot
  #' @export
  
  #libraray config
  source("R/config_plot.R")
  
  # load information about the labels: attention there are difference in the items between the time points
  keep_var_wave <- readr::read_delim(paste0(survey_pr, "var_names_mittagessen2_201124.csv"),
                              delim = ";",
                              col_names = TRUE, trim_ws = TRUE,
                              locale = readr::locale(encoding = "latin1")) %>% 
  dplyr::filter(!is.na(VAR_CONTENT)) %>% 
    dplyr::rename(questions = VAR_CONTENT, items = QUESTION)

  #stat
  df <- data %>% 
    dplyr::filter(stringr::str_detect(questions, pattern = pattrn)) %>% 
    # dplyr::filter(stringr::str_detect(PR, pattern = pr)) %>% 
    #answer is still charachter => format to boolean
    dplyr::mutate(answer = as.logical(.$answer)) %>% 
    dplyr::group_by(time, questions, PR) %>% 
    #sum all booleans
    dplyr::summarise(tot = sum(answer, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(., keep_var_wave[, c("questions", "LABEL")], by = c("questions")) %>% 
    #remove all up to : https://stackoverflow.com/questions/12297859/remove-all-text-before-colon  
    dplyr::mutate(label = stringr::str_remove(LABEL, pattern = ".*:"),
                  label = factor(stringr::str_trim(label))) %>% 
    #drop all zeros and one with no PR
    dplyr::filter(tot != 0) %>% 
    tidyr::drop_na(PR) %>% 
    dplyr::mutate(PR = dplyr::if_else(PR == "PR1", "PR1 (K)", PR)) #change one PR
  
  
  #order
  df_p <- df %>% 
    group_by(questions, label) %>% 
    summarise(tot_ord = sum(tot)) %>% 
    left_join(df, .)
    


  p <- ggplot2::ggplot(df_p, ggplot2::aes(x = reorder(stringr::str_wrap(label, wrap), tot_ord), 
                                          y = tot, fill = PR)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack(), width = .6) + 
    ggplot2::geom_text(ggplot2::aes(label = tot), 
              # hjust vs. vjust: https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want
                position = ggplot2::position_stack(vjust = .5), size = 18*converter,
              family = ggplot2::theme_get()$text$family) +
    ggplot2::guides(fill = guide_legend(title = "Personalrestaurant")) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::coord_flip() +  
    ggplot2::facet_wrap(~ time,  scales = "free_y", ncol = 1, strip.position = "left", drop = TRUE) +
    ggplot2::labs(y = ylab, x = xlab, subtitle = subtitle) +
    mytheme_facet

  if(save == TRUE){
  ggsave(paste0(survey_plot_pr, pr, str_sub(pattrn, start = -11, end = -4), "_", 
                                     format(Sys.Date(), "%Y_%m_%d"), ".pdf"),
         plot = p,
         width = width,
         height = height,
         device = cairo_pdf)
  
  message("plot ", paste0("plots/", pr, str_sub(pattrn, start = -11, end = -4),
                          "_", format(Sys.Date(), "%Y_%m_%d")),
          "PDF"," has been saved")
  
  ggsave(paste0(survey_plot_pr, pr, str_sub(pattrn, start = -11, end = -4), "_", 
                                     format(Sys.Date(), "%Y_%m_%d"), ".png"),
         plot = p,
         width = width,
         height = height,
         device = "png")
  
  message("plot ", paste0("plots/", pr, str_sub(pattrn, start = -11, end = -4),
                          "_", format(Sys.Date(), "%Y_%m_%d")),
          "png"," has been saved")
  }else{
  print(p)
}

}

message("function plot_item_boolean PR is ready")
