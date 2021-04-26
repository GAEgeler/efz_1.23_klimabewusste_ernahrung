# configurations concernig the paths inside the rproject

#load packages
library(here)

####
# raw data
#####

popularity <- here::here("raw data/popularity//")

# define path of PR (survey) data
survey_pr <- here::here("raw data/survey/PR//")

# define path of ASZ (survey) data
survey_asz <- here::here("raw data/survey/ASZ//")

# define path of PR (till data)
till_pr <- here::here("raw data/till_data/PR//")

# define path of ASZ (till data)
till_asz <- here::here("raw data/till_data/ASZ//")


####
# plots
#####


# define path of PR (survey) data
survey_plot_pr <- here::here("plots/survey/PR//")

# define path of ASZ (survey) data
survey_plot_asz <- here::here("plots/survey/ASZ//")

# define path of PR (till data)
till_plot_pr <- here::here("plots/till_data/PR//")

# define path of ASZ (till data)
till_plot_asz <- here::here("plots/till_data/ASZ//")
