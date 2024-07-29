library(tidyverse)
library(rvest)

df_wfpp_sites <- read_csv("data/DataViz/Daten_WFPP/IT_Projektseminar_data_modified_20211012.csv") %>% select(link) %>% pull(.)

df_wfpp <- read_csv("data/DataViz/Daten_WFPP/IT_Projektseminar_data_modified_20211012.csv") %>% select(-`...1`, -`...15`) #%>% 

test <- "https://wfpp.columbia.edu/person/abby-meehan/"

test_html <- read_html(test) %>% html_element(".author-credit") %>% html_text()


df <- map_df(df_wfpp_sites, function(i){
  print(i)
  authors <- read_html(i) %>% html_element(".author-credit") %>% html_text()
  tibble("link" = i, "authors_raw" = authors)
  
})

df_authors <- df %>% 
  mutate(author = trimws(authors_raw) %>% str_remove_all(., "\\n|\\t") %>% str_remove(., "by ")) %>% 
  select(-authors_raw)

df_wfpp_with_authors <- df_wfpp %>% 
  left_join(., df_authors)


clipr::write_clip(df_wfpp_with_authors)
