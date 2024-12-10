library(tidyverse)

# path_to_files <- "data/DataViz/Daten_WFPP/Filmografien_2024_0521/"
path_to_files <- "data/DataViz/Daten_WFPP/Filmografien_WFPP_v02_2024_1018/"

file_list <- list.files(path_to_files)

df_wfpp_filmography_wide <- map_df(file_list, function(file){
  df <- read_csv(paste0(path_to_files, file), show_col_types = FALSE, col_types = cols(.default = col_character())) %>% 
    mutate(filename = file) %>% 
    rename_with(., tolower, everything())
})

id_cols <- c("filename", "title", "archives", "unfinished", "uncertainty", "release_years", "release years", "production_company", "production_country")

df_wfpp_filmography_long <- df_wfpp_filmography_wide %>% 
  mutate(
  # archives = ifelse(is.na(Archives), archives, Archives),
         release_years = ifelse(!is.na(`release years`), `release years`, release_years)
  #        production_company = ifelse(is.na(production_company), Production_Company, production_company)
  ) %>% 
  select(!!!id_cols, everything(), -"release years") %>% #View()
  pivot_longer(., cols = "director":last_col(), names_to = "role", values_to = "value") %>% 
  filter(!is.na(value))
  

nr_persons <- df_wfpp_filmography_long %>% select(value) %>% distinct() %>% 
  mutate(nr_names = str_split(value, ",") %>% lengths()) %>% 
  arrange(desc(nr_names)) %>% head(1) %>% select(nr_names) %>% pull()

df_wfpp_filmography_long_persons <- df_wfpp_filmography_long %>% 
  separate(value, into = str_c("person_", 1:nr_persons), sep = ",") %>% 
  pivot_longer(., cols = starts_with("person_"), names_to = "persons", values_to = "actors") %>% 
  filter(!is.na(actors)) %>% 
  mutate(actors = trimws(actors),
         persons_bio = str_remove(filename, "\\.csv")
         ) %>% 
  mutate(counted_roles = group_rows(.), .by = c("persons_bio", "title", "role"))

df_wfpp_local_wd_ids <- read_csv("data/wikidata/wfpp-22-with-wikidata-ids.csv") %>% 
  mutate(name = ifelse(is.na(name), name_person, name))

#### Jetzt die filmografien mergen mit den biografischen daten -------------------------------------
## das merging passiert auch wieder auf basis aller Namen aller Akteure in den Filmografien. 
## hier sind jetzt also auch solche Personen mit ID enthalten, die im WFPP gelistet sind, aber nicht deren Filmografie das ist
df_wfpp_filmography_ids_ <- df_wfpp_filmography_long_persons %>% 
  left_join(., df_wfpp_local_wd_ids, by = c("actors" = "name")) #%>% 

### einschub neuer versuch 
df_wfpp_filmography_ids__ <- df_wfpp_local_wd_ids %>% 
  # fuzzyjoin::fuzzy_left_join(., df_wfpp_local_wd_ids, by = c("persons_bio" = "name"), match_fun = str_detect("name", "persons_bio")) #%>% 
  fuzzyjoin::regex_join(., df_wfpp_filmography_long_persons, by=c("name" = "persons_bio"), ignore_case=T) %>% 
  mutate(check = str_detect(tolower(actors), persons_bio)) %>% 
  filter(check)

neu_matching <- df_wfpp_filmography_ids__ %>% 
  filter(check)

found_files <- neu_matching %>% select(filename) %>% distinct()# %>% View()
missed_files <- setdiff(file_list, found_files$filename) %>% enframe() %>% select("filename" = "value")

missed_files_for_export <- df_wfpp_filmography_long_persons %>% 
  filter(filename %in% missed_files$filename) %>% 
  select(filename, persons_bio) %>% 
  distinct()

clipr::write_clip(missed_files_for_export)

handmatched_filenames <- read_tsv("data/DataViz/Daten_WFPP/matching-missing-filmographies.tsv") %>% 
  filter(persons_bio != "Kolm-Fleck") %>% 
  bind_cols(missed_files) %>% 
  select(-persons_bio)

prepared_missed_files <- df_wfpp_filmography_long_persons %>% 
  filter(filename %in% missed_files$filename) %>% 
  left_join(., handmatched_filenames, by = c("filename" = "filename...5")) %>% 
  select(-"filename...1")

df_wfpp_filmography_ids__2 <- df_wfpp_local_wd_ids %>% 
  left_join(., prepared_missed_files, by = c("name" = "match_name")) %>% 
  filter(!is.na(filename)) %>% #View()
  mutate(
    check = case_when(
      name == actors ~ TRUE,
      !is.na(match_actor) & match_actor == actors ~ TRUE,
      TRUE ~ FALSE
  )) %>% 
  filter(check)

test_actors_name_match <- df_wfpp_filmography_ids__2 %>% select(filename) %>% distinct() #%>% View()
test_diff <- setdiff(missed_files$filename, test_actors_name_match$filename)

df_wfpp_filmography_ids <- df_wfpp_filmography_ids__ %>% 
  bind_rows(., df_wfpp_filmography_ids__2) %>% 
  filter(!id_person %in% c("Q346150", "Q4588079")) %>% # durch die matching methode werden für die Nissen-Schwestern alle drei verknüpft, obwohl es nur eine Biografie für Aud gibt
  select(-counted_roles, -match_actor) %>% 
  mutate(role = ifelse(role == "adaptation", "screenwriter", role),
         uncertainty_2 = str_extract(actors, "\\(.*\\)$"),
         actors = str_remove(actors, "\\(.*\\)$") %>% trimws(),)

df_wfpp_filmography_ids %>% filter(role == "screenwriter", counted_roles > 1) %>% View()

# setdiff(file_list, df_wfpp_filmography_ids$filename)

write_csv(df_wfpp_filmography_ids, "data/doc/wfpp-filmography-ids.csv") 

# rm(df_merged_2)

load(file = "data/doc/df_merged_all.RData")

overlap_with_df_merged_all <- df_wfpp_filmography_ids %>% 
  select(id, name_person) %>% 
  distinct() %>% 
  filter(id %in% df_merged_all$id)



