library(tidyverse)
library(xml2)
library(rrapply)
library(stringi)

# list_xml_films <- xml2::as_list(xml2::read_xml("data/DataViz/DatenDFF/DFF_XML Abfragen_2022-03-21/DAVIF_ FW und P vor 1950 als XML_2022-03-21.xml", as_html = TRUE, encoding = "UTF-8"))
## Datennachlieferung
list_xml_films <- xml2::as_list(xml2::read_xml("data/DataViz/DatenDFF/DAVIF_FW und P als XML_2024-10-21.xml", as_html = TRUE, encoding = "UTF-8"))
list_xml_films_daten <- list_xml_films$html$body$data #%>% flatten() %>% as_tibble()

dff_films_xml <- rrapply::rrapply(
  list_xml_films_daten, 
  how = "melt", 
  options = list(namecols = TRUE, simplify =T)
)

### aus der xml-liste einen weiten df machen. problem: die id-spalten für personen und film heißen gleich, 
### deswegen wird hier an row_id und L2 gruppiert, um bei doppeltden Feldnamen ein _2 anzuhängen. 

df_dff_wider <- dff_films_xml %>% 
  select(L1, L2, value) %>% 
  as_tibble() %>% 
  mutate(row_id = ifelse(L2 == "fwid", paste0("row_", row_number()), NA), .by = c(L1,L2)) %>% 
  fill(row_id, .direction = "down") %>% #View()
  mutate(L2 = paste0(L2, "_", row_number()), .by = c(row_id, L2),
         L2 = str_remove(L2, "_1")) %>% #View()
  pivot_wider(id_cols = row_id, names_from = "L2", values_from = "value", values_fill = NA) #%>% 

### Datennachlieferung: Variablen heißen anders

df_dff_wider <- dff_films_xml %>% 
  select(L1, L2, value) %>% 
  as_tibble() %>% 
  mutate(row_id = ifelse(L2 == "person", paste0("row_", row_number()), NA), .by = c(L1,L2)) %>% 
  fill(row_id, .direction = "down") %>% #View()
  mutate(L2 = paste0(L2, "_", row_number()), .by = c(row_id, L2),
         L2 = str_remove(L2, "_1")) %>% #View()
  pivot_wider(id_cols = row_id, names_from = "L2", values_from = "value", values_fill = NA) %>% 
  rename(uid_2 = person, uid = filmwerk, idtitel_p = titel)


### die weiteren IDs für Deutsche Nationalbib oder Wikidata, stecken alle in einem Feld
## Wie viele IDs stecken maximal in einem Feld?

nr_ids_films <- df_dff_wider %>% 
  mutate(splitted = str_split(extids, "\\s") %>% lengths(.)) %>% 
  arrange(desc(splitted)) %>% head(1) %>% pull(.)

### longform alle IDs zu einer filmportal-ID
df_dff_film_ids <- df_dff_wider %>% 
  select(uid, extids) %>% 
  distinct() %>% 
  separate(col = extids, sep = "[\\s]", into = str_c("id_", 1:nr_ids_films)) %>% #View()
  # select(uid)
  pivot_longer(., cols = 2:12) %>% 
  filter(!is.na(value)) %>% 
  mutate(type = str_extract(value, "^.*:") %>% str_remove(., ":")) %>% 
  filter(value != "NULL") %>% 
  mutate(value = str_remove(value, "^.*:"))

df_dff_film_ids <- df_dff_film_ids %>% 
  pivot_wider(id_cols = c(uid, idname), names_from = type, values_from = value, values_fn = function(i){paste0(i, collapse = ",")}) %>% 
  rename("filmportal_film_id" = "uid")

write_csv(df_dff_film_ids, file = "data/ids/df_dff_film_ids.csv")


df_dff_film_occupation <- df_dff_wider %>% 
  select(uid_2, idname, rel, jahr, uid, idtitel_p) %>% 
  distinct()


write_csv(df_dff_film_occupation, file = "data/dff/df_dff_person_occupation_second_export.csv")

### in dem Datensatz der Filme sind ebenfalls alle weiterführenden IDs der Personen enthalten
## Wie viele IDs stecken max in einem Feld?
### diese IDs sind viel zu wild um sie zu verwenden. Das Parsen der IDs passiert auf Basis der personen-xml weiter unten.
nr_ids_person <- df_dff_wider %>%
  filter(geschlecht != "M") %>%
  select(-geschlecht) %>%
  mutate(splitted = str_split(extids_2, "\\s") %>% lengths(.)) %>%
  arrange(desc(splitted)) %>% head(1) %>% pull(.)

df_dff_wider %>%
  select(uid_2, idname, extids_2, geschlecht) %>%
  distinct() %>%
  filter(geschlecht != "M") %>%
  mutate(doubled = row_number(), .by = uid_2)%>% View()

df_dff_person_ids <- df_dff_wider %>%
  select(uid_2, idname, extids_2, geschlecht) %>%
  distinct() %>%
  filter(geschlecht != "M") %>% #View()
  select(-geschlecht) %>%
  separate(col = extids_2, sep = "[\\s]", into = str_c("id_", 1:nr_ids_person)) %>% #View()
  pivot_longer(. , cols = 1:nr_ids_person+2) %>%
  filter(!is.na(value)) %>%
  mutate(type = str_extract(value, "^.*:") %>% str_remove(., ":")) %>%
  filter(value != "NULL") %>%
  mutate(value = str_remove(value, "^.*:")) %>%
  pivot_wider(id_cols = c(uid_2, idname), names_from = type, values_from = value, values_fn = function(i){paste0(i, collapse = ",")}) %>%
  rename("filmportal_person_id" = "uid_2", "name" = "idname")

write_csv(df_dff_person_ids, file = "data/ids/dff-person-ids-with-filmography.csv")

df_dff_person_ids %>% select(filmportal_person_id) %>% distinct() %>% nrow()


# read person xmls --------------------

list_xml_person <- xml2::as_list(xml2::read_xml("data/DataViz/DatenDFF/DFF_XML Abfragen_2022-03-21/DAVIF_ Personen vor 1950 als XML_2022-03-21.xml", as_html = TRUE, encoding = "UTF-8"))
list_xml_person_daten <- list_xml_person$html$body$data #%>% flatten() %>% as_tibble()

dff_person_xml <- rrapply::rrapply(
  list_xml_person_daten, 
  how = "melt", 
  options = list(namecols = TRUE, simplify =T)
)


### biografische Daten der Personen parsen
df_dff_wider_person <- dff_person_xml %>% 
  select(L1, L2, value) %>% 
  as_tibble() %>% 
  mutate(row_id = ifelse(L2 == "pid", paste0("row_", row_number()), NA), .by = c(L1,L2)) %>% 
  fill(row_id, .direction = "down") %>% #View()
  pivot_wider(id_cols = row_id, names_from = "L2", values_from = "value", values_fill = NA, values_fn = function(i){paste0(i, collapse = ",")}) %>% 
  unnest(cols = c(pid, uid, idname, extids, geburtsdatum, geburtsort, sterbedatum, sterbeort, taetigkeiten, taetigzeit, laender, geschlecht, anmerkungen, anmerkunglokal)) %>% 
  filter(geschlecht != "M") %>%
  mutate(across(where(is.character), ~na_if(., "NULL"))) %>% 
  mutate(year_born = str_extract(geburtsdatum, "^[1-9]{4,}")) %>% 
  rename("filmportal_id" = "uid", "name_person" = "idname")

df_dff_wider_person %>% select(uid) %>% distinct() %>% nrow()

write_csv(df_dff_wider_person, file = "data/dff/dff-person-bio.csv")

df_dff_local_person_year_born <- df_dff_wider_person %>% 
  select(filmportal_id, name_person, year_born) %>% 
  distinct()

write_csv(df_dff_local_person_year_born, file = "data/dff/dff-parsed-person-year-born.csv")

### ids extrahieren

nr_ids_person <- df_dff_wider_person %>% 
  filter(geschlecht != "M") %>% 
  select(-geschlecht) %>% 
  mutate(splitted = str_split(extids, "\\s") %>% lengths(.)) %>% 
  arrange(desc(splitted)) %>% head(1) %>% pull(.)


df_dff_person_ids <- dff_person_xml %>% 
  select(L1, L2, value) %>% 
  as_tibble() %>% 
  mutate(row_id = ifelse(L2 == "pid", paste0("row_", row_number()), NA), .by = c(L1,L2)) %>% 
  fill(row_id, .direction = "down") %>% #View()
  pivot_wider(id_cols = row_id, names_from = "L2", values_from = "value", values_fill = NA, values_fn = function(i){paste0(i, collapse = ",")}) %>% 
  unnest(cols = c(pid, uid, idname, extids, geburtsdatum, geburtsort, sterbedatum, sterbeort, taetigkeiten, taetigzeit, laender, geschlecht, anmerkungen, anmerkunglokal)) %>% 
  filter(geschlecht != "M") %>% #View()
  select(row_id, uid, idname, extids) %>% #View()
  separate(col = extids, sep = "[\\s]", into = str_c("id_", 1:nr_ids_person)) %>% #View()
  pivot_longer(. , cols = 1:nr_ids_person+3) %>% #View()
  filter(!is.na(value)) %>% 
  mutate(type = str_extract(value, "^.*:") %>% str_remove(., ":")) %>% 
  filter(value != "NULL") %>% 
  mutate(value = str_remove(value, "^.*:"),
         idname = trimws(idname)) %>% 
  pivot_wider(id_cols = c(row_id, uid, idname), names_from = type, values_from = value, values_fn = function(i){paste0(i, collapse = ",")}) %>% 
  rename("filmportal_person_id" = "uid", "name" = "idname") %>% 
  select(-row_id)

write_csv(df_dff_person_ids, file = "data/ids/dff-person-ids.csv")

df_dff_person_ids %>% select(filmportal_person_id) %>% distinct() %>% nrow()

