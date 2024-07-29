library(tidyverse)
library(tidywikidatar)
library(WikidataQueryServiceR)


#### Testpersonen 
## wfpp intern: 16546
## wfpp wikidata: Q3290731
## name: marguerite-renoir

## wfpp intern: 16546 ## sind gleich, diese hier ist falsch
## wffp intern soll: 15938
## wfpp wikidata: Q55806876
## name: mrs-m-t-pender



### alle frauen, die mit einer womens film pionier id auf wiki data gefunden werden
df_wfpp_wikidata_ids <- read_csv("data/wikidata/women-wfpp.csv", show_col_types = FALSE) %>% 
  mutate(id_person = str_extract(item, "Q.*$")) %>% rename("name_person" = "itemLabel")

### die erste lieferung der wfpp-daten enthält ids, deswegen wird sie hier eingelesen
df_wfpp_old <- read_csv("data/DataViz/Daten_WFPP/IT_Projektseminar_data_modified_20211012.csv", show_col_types = FALSE) %>% 
  select(-`...1`, -`...15`) %>% 
  mutate(YOB = as.numeric(YOB),
         name = trimws(name))

### die neuere datei mit mehr einträgen ist die folgende
df_wfpp_new <- read.csv("data/DataViz/Daten_WFPP/WFPP_metadata_July 10_2022.csv", sep = ";", na.strings = c("", " "))
## die zeilen code, um diejenigen frauen zu finden, die in der neueren mehr enthalten sind und deswegen keine id hatten, fehlen hier
## pauline hat die liste an kate geschickt, die ihrerseits ids zurück geschickt hat. 
## die werden in der nächsten zeile geladen
df_wfpp_new_ids <- read.csv("data/DataViz/Daten_WFPP/wfpp-new-ids.csv", sep = ";")

### aus den beiden datensätzen wird einer gemacht
df_wfpp_new_id <- df_wfpp_old %>% select(id, name) %>% 
  right_join(., df_wfpp_new, by = c("name" = "Name")) %>% 
  left_join(., df_wfpp_new_ids, by = "name") %>% 
  mutate(id.x = ifelse(is.na(id.x), id.y, id.x),
         name = trimws(name)) %>% 
  rename("id" = "id.x") %>% select(-id.y)

### die aka-names der frauen werden weiter unten gebraucht
aka_names <- df_wfpp_new_id %>%
  select(id, name, Also.known.as) %>% 
  mutate(M1 = strsplit(Also.known.as, "[,]")) %>%
  unnest(M1) %>%
  group_by(id) %>%
  mutate(v = paste0("M1.", row_number())) %>% #View()
  pivot_wider(id, names_from = "v", values_from = "M1") %>%
  ungroup() %>% #View()
  pivot_longer(., cols = 2:last_col(), names_to = "columns", values_to = "name", values_drop_na = TRUE) %>% 
  mutate(name = trimws(name)) %>% 
  select(-columns)

all_names_local <- df_wfpp_new_id %>% 
  select(id, name) %>% 
  bind_rows(., aka_names) %>% 
  distinct()


### alias namen für wikidata ids

get_aliases_for <- df_wfpp_wikidata_ids %>% select(id_person) %>% pull(.)

wikidata_alias_names <- map_df(get_aliases_for, function(i){
  alias_names <- query_wikidata(paste0("SELECT DISTINCT 
  ?item ?itemLabel ?itemAltLabel 
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en, de, fr, it, cz, ru'. }
    {
      BIND(wd:", i, " as ?item)     }
  }"))
})

aka_names_wikidata <- wikidata_alias_names %>% 
  mutate(id_person =  str_extract(item, "Q.*$")) %>% 
  rename("name" = "itemLabel") %>% select(-item) %>% 
  mutate(M1 = strsplit(itemAltLabel, "[,]")) %>%
  unnest(M1) %>%
  group_by(id_person) %>%
  mutate(v = paste0("M1.", row_number())) %>% #View()
  pivot_wider(id_person, names_from = "v", values_from = "M1") %>%
  ungroup() %>% #View()
  pivot_longer(., cols = 2:last_col(), names_to = "columns", values_to = "name", values_drop_na = TRUE) %>% 
  mutate(name = trimws(name)) %>% 
  select(-columns)

all_names_wikidata <- df_wfpp_wikidata_ids %>% 
  select(id_person, "name" = "name_person") %>% 
  bind_rows(., aka_names_wikidata) %>% 
  distinct()

### hier startet das matching um lokale daten mit wikidata-ids zu versehen. 
### theoretisch fehlen nur 2 Personen, auf die Gesamtsumme betrachtet. da ich aber an den namen matchen braucht es mehrere schritte

matches_local_wikidata <- all_names_wikidata %>% 
  inner_join(., all_names_local) %>% 
  # select(-name) %>%
  distinct()

### das matching an den namen erzeugt falsche verbindungen 
matches_local_wikidata_check <- matches_local_wikidata %>% 
  mutate(sum_local = n(), .by = id) %>% ## zählen wie viele gleiche lokale ids es gibt
  mutate(sum_wikidata = n(), .by = id_person) %>% ## das gleiche für die wikidata ids
  mutate(same = ifelse(sum_local == sum_wikidata, TRUE, FALSE)) %>% ## checken, ob die zahlen sich unterscheiden, denn das dürften sie nicht, wenn das matching glatt gegangen wäre
  filter(!same) %>% ## nur die jenigen anschauen, bei denen es unterschiede gibt
  select(-name) %>% distinct() %>% ## und die namen aus dem lokalen datensatz dazu matchen, für bessere lesbarkeit
  left_join(., df_wfpp_new_id) %>% select(-Also.known.as, -Worked.As, -Worked.In, -Permalink) %>% 
  arrange(name)

## auflösen lässt sich das nicht regelbasiert, jedenfalls fällt mir nix ein, ich mache das schnell händisch
matches_local_wikidata_check %>% clipr::write_clip()
matches_local_wikidata_checked <- read_tsv("data/ids/wfpp-name-matching-solved.tsv")

## jetzt den vorherigen datensatz säubern
matches_local_wikidata_cleaned <- matches_local_wikidata %>% 
  # left_join(., sum_matches_local) %>% 
  # left_join(., sum_matches_wikidata) %>% 
  mutate(sum_local = n(), .by = id) %>% ## zählen wie viele gleiche lokale ids es gibt
  mutate(sum_wikidata = n(), .by = id_person) %>% ## das gleiche für die wikidata ids
  mutate(same = ifelse(sum_local == sum_wikidata, TRUE, FALSE)) %>% ## checken, ob die zahlen sich unterscheiden, denn das dürften sie nicht, wenn das matching glatt gegangen wäre
  filter(same) %>%
  bind_rows(., matches_local_wikidata_checked) %>% 
  select(id_person, id) %>% 
  distinct()


## matching an namen bedeutet auch, dass frauen fehlen können, wenn bspw. sprachenspezifische buchstaben verwendet werden und mal nicht
## welche der lokalen frauen haben keine entsprechung in wikidata, auf basis des namen-matchings?
missing_ids_local <- df_wfpp_new_id %>% 
  filter(!id %in% matches_local_wikidata_cleaned$id)

## welche der wikidata frauen hat keine entsprechung im lokalen datensatz, auf basis des namen-matchings?
missing_ids_wikidata <- df_wfpp_wikidata_ids %>% 
  filter(!id_person %in% matches_local_wikidata_cleaned$id_person) %>% 
  arrange(name_person) %>% select(-item)

### copy & paste für händisches matching

matches_local_wikidata_cleaned %>% clipr::write_clip()
missing_ids_local %>% clipr::write_clip()
missing_ids_wikidata %>% clipr::write_clip()

### als tsv gespeichert
df_matches_handmatched <- read_tsv("data/ids/wfpp-missing-handmatched.tsv") %>% select(id_person, id)
matches_local_wikidata_cleaned <- matches_local_wikidata_cleaned %>% 
  bind_rows(., df_matches_handmatched) %>% distinct()


### datensatz mit lokalen und wikidata Pionierinnen erstellen

df_matches_local_wikidata <- matches_local_wikidata_cleaned %>% 
  full_join(., df_wfpp_new_id, by = "id") %>% 
  full_join(., df_wfpp_wikidata_ids, by = "id_person")

write_csv(df_matches_local_wikidata, file = "data/wikidata/wfpp-22-with-wikidata-ids.csv")
