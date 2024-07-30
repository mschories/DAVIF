library(tidyverse)

## Dieses script führt die Datensätze zusammen
### zunächst werden die biografischen Datensätze von WFPP und DFF verbunden
### danach auf die filmografischen Übereinstimmungen mit den bisher erhobenen WFPP-Filmen heruntergefiltert 
### die DFF-Filme sind vollständig mit den DFF-biografischen Daten abgedeckt, das zeigen die letzten Zeilen

# biografische Daten einlesen ------------------------------------------------

# für die wfpp-Frauen habe ich schon eine Datei erstellt, in der zu den biografischen daten die wikidata-ids schon hinzugefügt sind
# https://github.com/mschories/DAVIF/blob/main/analyse/make-wfpp-ids.R
wfpp_with_wd_ids <- read_csv("data/wikidata/wfpp-22-with-wikidata-ids.csv", show_col_types = FALSE)

# https://github.com/mschories/DAVIF/blob/main/gather/dff-read-xml.R
dff_all_persons_bio <- read_csv("data/dff/dff-person-bio.csv", show_col_types = FALSE)

df_dff_wikidata <- read_csv("data/wikidata/women-filmportal.csv", show_col_types = FALSE) %>% 
  as_tibble() %>% 
  mutate(value = toupper(value)) %>% 
  # filter(value %in% df_dff_pioniers$Person)
  filter(value %in% dff_all_persons_bio$filmportal_id)


# start merging bio data -----------------------------------------------------------

# die beiden folgenden Datensätze verwenden unterschiedliche Namensspalten aus dem WFPP-Datensatz
# dieser enthält die Namen, die in der lokalen Daten vom WFPP bereitgestellt wurde 
# und zusätzlich die Namen, die in Wikidata gelistet sind

df_merged_1 <- wfpp_with_wd_ids %>% 
  mutate(name = ifelse(is.na(name), name_person, name)) %>% 
  left_join(., dff_all_persons_bio, by = c("name_person" = "name_person")) %>% 
  filter(!is.na(filmportal_id))

df_merged_2 <- wfpp_with_wd_ids %>% 
  mutate(name = ifelse(is.na(name), name_person, name)) %>% 
  left_join(., dff_all_persons_bio, by = c("name" = "name_person")) %>% 
  filter(!is.na(filmportal_id))

overlap_ <- df_merged_1 %>% filter(name %in% df_merged_2$name)
antijoin_1 <- anti_join(df_merged_1, df_merged_2)
antijoin_2 <- anti_join(df_merged_2, df_merged_1)

## nächster check: gibt es übereinstimmungen über die aka namen?
# yes, eine wird gefunden!

aka_names <- df_wfpp %>%
  mutate(M1 = strsplit(Also.known.as, "[,]")) %>%
  unnest(M1) %>%
  group_by(id) %>%
  mutate(v = paste0("M1.", row_number())) %>% #View()
  pivot_wider(id, names_from = "v", values_from = "M1") %>%
  ungroup() %>% #View()
  pivot_longer(., cols = 2:last_col(), names_to = "colums", values_to = "name", values_drop_na = TRUE) %>% 
  mutate(
    name = str_remove(name, "\\(.*\\)"),
    name = str_remove_all(name, '\\“|\\"|\\”'),
    name = str_remove(name, "\\.$"),
    name = trimws(name)
  )

df_merged_3 <- aka_names %>% 
  select(id, aka_name = name) %>% 
  left_join(., wfpp_with_wd_ids) %>% 
  left_join(., dff_all_persons_bio, by = c("aka_name" = "name_person") ) %>% 
  filter(!is.na(filmportal_id)) %>% 
  mutate(matched = "aka_names") %>% 
  select(-aka_name)

# aka_names_matched <- wfpp_with_wd_ids %>% 
#   filter(id %in% df_merged_3$id)

df_merged_names <- df_merged_1 %>% 
  bind_rows(antijoin_2) %>% distinct() %>% 
  mutate(matched = "names") %>% 
  bind_rows(., df_merged_3) #%>% 

## next step: checking on wd-id 

dff_wikidata_ids <- df_dff_wikidata %>% 
  mutate(id_person = str_extract(item, "Q.*$")) %>% 
  rename("name_person" = "itemLabel", "filmportal_id" = "value") %>% 
  select(-item)

df_merged_wd_ids <- dff_wikidata_ids %>% 
  full_join(., wfpp_with_wd_ids, by = join_by(name_person, id_person)) %>% 
  filter(!is.na(id), !is.na(filmportal_id)) %>% 
  distinct() %>% 
  mutate(matched = "wdid")

## sind in dem wikidata-id-merge noch personen dabei, die vorher nicht dabei waren?
additional_woman_via_wd <- df_merged_wd_ids %>% filter(!filmportal_id %in% df_merged_names$filmportal_id)

temp_data_wfpp <- wfpp_with_wd_ids %>% 
  filter(id %in% additional_woman_via_wd$id) %>% arrange(name)

temp_data_dff <- dff_all_persons_bio %>% 
  filter(filmportal_id %in% additional_woman_via_wd$filmportal_id) %>% arrange(name_person)

temp_data_merged  <- temp_data_wfpp %>% 
  select(-name_person) %>% 
  bind_cols(temp_data_dff) %>% 
  mutate(matched = "wd_id")

df_merged_bio <- df_merged_names %>% 
  bind_rows(., temp_data_merged) %>% 
  distinct()

save(df_merged_bio, file = "data/doc/df_merged_bio.RData")

load(file = "data/doc/df_merged_bio.RData")
# merge aus allen vier Datenquellen, also biografische Daten plus Filminformationen ---------------------------------------------

dff_film <- read_csv(file = "data/dff/df_dff_person_occupation.csv", show_col_types = F)

wfpp_filmography_ids <- read_csv("data/doc/wfpp-filmography-ids.csv", show_col_types = FALSE)

wfpp_filmography_ids %>% select(id) %>% distinct() %>% nrow()

df_merged_bio_film <- df_merged_bio %>% 
  filter(id %in% wfpp_filmography_ids$id)

save(df_merged_bio_film, file = "data/doc/df_merged_bio_film.RData")

# test ---------------------------------

## test, ob auch wirklich alle personen, aus der dff-film datei in der biografischen datei vom dff schon genannt sind
## ja, sind alle enthalten
dff_persons_overlap_bio_film <- dff_film %>% 
  filter(uid_2 %in% dff_all_persons_bio$filmportal_id) %>% 
  select(uid_2) %>% distinct()

dff_persons_overlap_bio_film_ <- dff_all_persons_bio %>% 
  filter(filmportal_id %in% dff_film$uid_2) %>% 
  select(filmportal_id) %>% distinct()
