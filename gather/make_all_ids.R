library(tidyverse)


#### dff special ids 

## df kommt aus dff-read-xml-film
df_dff_local <- read.csv("data/ids/dff-person-ids.csv") %>% 
  rename_with(~paste0("local_", ., recycle0 = TRUE)) %>% 
  rename("filmportal_id" = "local_filmportal_person_id", local_dff_name = local_name)

## df kommt aus wikidata-biografies.R
df_dff_online <- read.csv("data/ids/dff-woman-wikidata-cmprsd.csv") %>% 
  mutate(filmportal_id = toupper(filmportal_id)) %>% 
  rename_with(~paste0("online_", ., recycle0 = TRUE)) %>% 
  rename("filmportal_id" = "online_filmportal_id", online_dff_name = online_name)

df_dff_all_ids <- df_dff_local %>% 
  full_join(., df_dff_online) #%>% 
  # mutate(name = ifelse(is.na(local_name), online_name, local_name)) %>% 
  # select(-online_name, -local_name)
  # rename("name" = "local_name")

df_dff_all_ids %>%  filter(is.na(name)) %>% View()

#### wfpp special ids

df_wfpp_online <- read_csv("data/ids/wfpp-woman-wikidata.csv")

## diese Tabelle ist schon ein join mit online ids (wikidata), bisher allerdings noch keine Permalinks berücksichtigt.
# für die wfpp-daten werden die Permalinks auf wikipedia als externe Links in das Archiv verwendet
df_wfpp_local <- read.csv("data/wikidata/wfpp-22-with-wikidata-ids.csv") %>% 
  select(wd_id = id_person, local_wfpp_id_local = id, Permalink, local_wfpp_name = name) %>% 
  mutate(local_wfpp_id = trimws(Permalink) %>% str_remove(., "/$") %>% str_extract(., "/[a-z-123]{1,}$") %>% str_remove(., "/")) %>% 
  select(-Permalink) %>% 
  rename(online_wd_id = wd_id)

df_wfpp_all_ids <- df_wfpp_online %>% 
  full_join(., df_wfpp_local) %>% 
  mutate(online_viaf_id = as.character(online_viaf_id),
         online_filmportal_id = toupper(online_filmportal_id))

df_wfpp_all_ids %>% select(online_wd_id, local_wfpp_id_local) %>% distinct() %>% nrow()

## 
# rm(df_all_women_ids)

df_all_women_ids_ <- df_wfpp_all_ids %>% 
  full_join(., df_dff_all_ids, relationship = "many-to-many",by = join_by(online_wd_id, online_wfpp_id, online_imdb_id, online_viaf_id, online_gnd_id, online_filmportal_id == filmportal_id)) %>% 
  rename(local_gnd_id = local_GND, local_wd_id = local_WD, local_viaf_id = local_VIAF) %>% 
  select(-local_NA.) %>% #View()
  select(online_filmportal_id, everything()) %>% 
  mutate(across(online_wd_id:local_viaf_id, ~as.character(.x)),
         online_wfpp_id_local = NA,
         local_imdb_id = NA, ## wird hinzugefügt, fehlt in den daten
         local_filmportal_id = online_filmportal_id, # wird auch hinzugefügt, nur aus symetriegründen, eigentlich nicht notwendig
         online_viaf_id = ifelse(str_detect(online_viaf_id, "(NA\\|){1,}"), NA, online_viaf_id),
         online_gnd_id = ifelse(str_detect(online_gnd_id, "(NA\\|){1,}"), NA, online_gnd_id)
         # name = ifelse(is.na(name.x), name.y, name.x)
         ) %>% 
  distinct() #%>% 
  
### warum ich hier unbedingt die geburtsjahre noch dazu haben wollte, leuchtet heute nicht mehr ein. 
### für eine grafik, die über die zeit hinweg einen verlauf zeichnen könnte, so viel ist klar. 
### aber während des prozesses wurde deutlich, wie wenig eindeutig die daten sind und gegenwärtig gibt es auch keine solche visualisierung
### es ist extrem fehleranfällig, deswegen ist es hier auskommentiert
  # left_join(., df_dff_person_year_born, by = c("local_filmportal_id" = "filmportal_id")) %>% 
  # left_join(., df_wfpp_person_year_born, by = c("online_wd_id" = "id_person", "local_wfpp_id_local" = "id")) %>% View()
  # mutate(year_born = ifelse(is.na(year_born.x), year_born.y, year_born.x),
  #        name = ifelse(is.na(name), name.x.x, name),
  #        name = ifelse(is.na(name), name.y.y, name)) %>% #View()
  # select(-year_born.x, -year_born.y, -name.x.x, -name.y.y, -name.y, -name.x) %>%
  # distinct() %>%
  # filter(!is.na(name))
  # View()
  
df_all_women_ids <- df_all_women_ids_ %>% 
  mutate(id_data = paste0("row_", row_number())) %>% 
         # wfpp = ifelse()) %>% 
  pivot_longer(cols = online_filmportal_id:local_filmportal_id, names_to = "id", values_to = "value") %>% 
  mutate(type = str_extract(id, "^online_|local_") %>% str_remove(., "_"),
         id_cleaned = str_remove(id, "^online_|local_"),
         present = ifelse(!is.na(value), TRUE, FALSE))


### weil das mit den ids so unglaublich mühsam ist, erstelle ich hier einen df in dem die werte der 
### ids durch true and false ersetzt werden. es kommt viel zu häufig vor, dass für eine person unterschiedliche ids hinterlegt sind. 
### das wird hier verschleiert, wenn es nur darauf ankommt, ob die person dokumentiert ist
df_all_women_ids_present <- df_all_women_ids_ %>% 
  select(name, year_born, everything()) %>% #filter(is.na(name))%>% View()
  distinct() %>% 
  mutate(
    id_data =  paste0("row_", row_number()),
    wfpp = ifelse(!is.na(local_wfpp_id_local), TRUE, FALSE),
    dff =  ifelse(!is.na(local_filmportal_id), TRUE, FALSE)) %>% 
  mutate(across(online_filmportal_id:local_filmportal_id, ~ifelse(!is.na(.x), TRUE, FALSE))) %>% 
  pivot_longer(cols = online_filmportal_id:local_filmportal_id, names_to = "id", values_to = "value") %>% 
  mutate(type = str_extract(id, "^online_|local_") %>% str_remove(., "_"),
         id_cleaned = str_remove(id, "^online_|local_"))
  


write_csv(df_all_women_ids_, file = "data/ids/df_all_women_ids_wide.csv")
save(df_all_women_ids, file = "data/ids/df_all_women_ids.RData")
save(df_all_women_ids_present, file = "data/ids/df_all_women_ids_present.RData")


#### ab hier spielerei, bisher nicht verwendet

df_dff_local_person_year_born <- read.csv(file = "data/dff/dff-parsed-person-year-born.csv") %>% filter(!is.na(year_born))
df_dff_online_person_year_born <- read_csv(file = "data/wikidata/dff-bio-wikidata.csv") %>% 
  select(filmportal_id, name_person, date_of_birth) %>% 
  mutate(year_born = str_extract(date_of_birth, "^\\+[0-9]{4,}") %>% str_remove(., "\\+"),
         filmportal_id = toupper(filmportal_id)) %>% filter(!is.na(year_born))

df_dff_person_year_born <- df_dff_local_person_year_born %>% 
  left_join(., df_dff_online_person_year_born, by = join_by(filmportal_id, name_person)) %>% 
  mutate(year_born = ifelse(!is.na(year_born.x), year_born.x, year_born.y)) %>% 
  select(filmportal_id, name = name_person, year_born)


wfpp_local_ids_for_joining <- read.csv("data/wikidata/wfpp-22-with-wikidata-ids.csv") %>% 
  select(id_person, id, name)

df_wfpp_local_person_year_born <- read.csv("data/DataViz/Daten_WFPP/IT_Projektseminar_data_modified_20211012.csv") %>% 
  select(id, name, YOB) %>% 
  full_join(wfpp_local_ids_for_joining)

# df_all_women_ids %>% 
#   filter(id == "local_wfpp_id_local", !is.na(value)) %>% View()

df_wfpp_online_person_year_born <- read.csv("data/wikidata/wfpp-bio-wikidata.csv") %>% 
  select(id_person, name_person, date_of_birth) %>% distinct() %>% 
  mutate(year_born = str_extract(date_of_birth, "^\\+[0-9]{4,}") %>% str_remove(., "\\+"))

df_wfpp_person_year_born <- df_wfpp_local_person_year_born %>% 
  full_join(df_wfpp_online_person_year_born) %>% 
  mutate(year_born = ifelse(is.na(YOB), year_born, YOB),
         name = ifelse(is.na(name), name_person, name),
         id = as.character(id)) %>% 
  select(id_person, id, name, year_born) %>% 
  filter(!is.na(year_born))

df_all_women_ids %>% 
  filter(!is.na(name)) %>% 
  head(11200) %>% 
  ggplot(., aes(x = id_cleaned, y = type, fill = present)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "lightgrey")) +
  facet_wrap(~name)










