# read meal menus from the novanimal experiment (university canteen)--------

# status: september 2020
# author: gian-andrea egeler

# read data from zenodo, for more info see: https://zenodo.org/record/3890931#.X7WIr2hKg2w

selling_PR6 <- readr::read_delim("https://zenodo.org/record/3890931/files/2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv?download=1", 
                             delim = ";") %>% 
  group_by(date, article_description, meal_name_comp, label_content) %>% 
  summarise(tot_sold = n()) %>% 
  rename(meal_component = meal_name_comp, meal_label = label_content, meal_line = article_description) %>% 
  mutate(source = "PR6") %>% 
  drop_na(meal_component)
 