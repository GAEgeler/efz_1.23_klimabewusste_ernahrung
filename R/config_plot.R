# configurations for the plots


#defining mytheme
extrafont::loadfonts(device = "win", quiet = TRUE)


#that geom_text inherits from theme: https://stackoverflow.com/questions/48977963/how-to-let-geom-text-inherit-theme-specifications-ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_family = "Calibri"))
ggplot2::update_geom_defaults("text", list(family = ggplot2::theme_get()$text$family))


#for the most plots
mytheme <- ggplot2::theme_bw()+ # definve theme for plot
  ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"),
        axis.text.x = ggplot2::element_text(size=22),
        axis.text.y = ggplot2::element_text(size=22, face = "plain"),
        legend.text = ggplot2::element_text(size = 22),
        legend.title = ggplot2::element_text(size =22),
        strip.text = ggplot2::element_text(size=22),
        strip.background = ggplot2::element_rect(colour="black",
                                        fill="grey90"), #background color for facet wraps
        panel.spacing = ggplot2::unit(1, "lines"), # space between panels 
        axis.title.y = ggplot2::element_text(size = 22, margin = ggplot2::margin(t = 0, r = 22, b = 0, l = 0)),
        axis.title.x = ggplot2::element_text(size = 22,  margin = ggplot2::margin(t = 22, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b=15), size = 22),
        plot.caption = ggplot2::element_text(margin = ggplot2::margin(t=15), face="italic", size=22),
        text = ggplot2::element_text(family = ggplot2::theme_get()$text$family),
        # legend.key = ggplot2::element_rect(color = "white", size = 6, fill = "white"), # see for that part the funktion draw_key_ploygon3
        # legend.key.size = ggplot2::unit(1.5, "cm"),
        legend.margin = ggplot2::margin(-0.5, 0, 0.05, 0, "cm"),
        plot.margin = ggplot2::unit(c(t = 0, r = 0, b = 0, l = 0),"cm"))


#for plots with double x-axis: https://stackoverflow.com/questions/52554822/save-a-ggplot2-time-series-plot-grob-generated-by-ggplotgrob
mytheme_facet <- ggplot2::theme_classic()+ # definve theme for plot
  ggplot2::theme(axis.text.x = ggplot2::element_text(size=22),
        axis.text.y = ggplot2::element_text(size=22, face = "plain"),
        #remove the lines, background and grid
        # panel.border = ggplot2::element_blank(),
        # panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        # panel.grid = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 22),
        legend.title = ggplot2::element_text(size =22),
        strip.text = ggplot2::element_text(size=22),
        strip.background.x = ggplot2::element_blank(), #background color to blanc facet wraps
        # remove facet spacing on x-direction
        strip.placement = 'outside',
        panel.spacing.x = ggplot2::unit(0, "lines"), 
        axis.title.y = ggplot2::element_text(size = 22, margin = ggplot2::margin(t = 0, r = 22, b = 0, l = 0)),
        axis.title.x = ggplot2::element_text(size = 22,  margin = ggplot2::margin(t = 22, r = 0, b = 0, l = 0)),
        plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b=15),size = 30),
        plot.caption = ggplot2::element_text(margin = ggplot2::margin(t=15), face="italic", size=22),
        text = ggplot2::element_text(family = ggplot2::theme_get()$text$family)) #,
        # legend.key.size = unit(1.5, "cm"),
        # legend.margin = margin(-0.5, 0, 0.05, 0, "cm"),
        # plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0),"cm"))


  
#set coord expansion
#source: https://stackoverflow.com/questions/61969752/force-the-origin-to-start-at-0-without-margin-between-data-and-x-axis-in-new-ggp

scale_y_origin <- function(...) {
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)), ...)
}


#geom_text size "converter": https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size
converter = (25.4/72.27)
