library(tidyverse)
library(tidywikidatar)
library(purrr)
tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)




## Der erste Teil des Scripts geht von einer Frau aus und überprüft, ob es Einträge gibt, die mit ihr verknüpft sind. 
## Bedingung für einen solchen Eintrag ist, dass er der unterkategorie von Film angehört
# https://query.wikidata.org/#SELECT%20DISTINCT%20%3Fitem%20%3FitemLabel%20%3Fp%20%3FrelationLabel%20%3Fvalue%20%3FvalueLabel%20WHERE%20%7B%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%20%7D%0A%20%20%7B%0A%20%20%20%20BIND%28wd%3AQ234795%20as%20%3Fvalue%29%20%0A%20%20%20%20%3Fitem%20%3Fp%20%3Fvalue.%0A%20%20%20%20%3Fitem%20%28wdt%3AP31%2Fwdt%3AP279%2a%29%20wd%3AQ11424.%0A%20%20%20%20%3Frelation%20wikibase%3AdirectClaim%20%3Fp%20.%0A%20%20%7D%0A%7D
# https://github.com/lvaudor/glitter
# https://github.com/wikimedia/WikidataQueryServiceR
## notes: https://github.com/cutterkom/remove-na-lgbtiq-queer-knowledge-graph/blob/main/apps/companions/queries.R#L70C1-L84C2

library(WikidataQueryServiceR)

 
get_filmography <-  function(archive, df_mapping){
  filmography <- map_df(df_mapping, function(i){
    filmography_query <- query_wikidata(paste0("SELECT DISTINCT 
    ?item ?itemLabel ?p ?relationLabel ?value ?valueLabel 
    WHERE {
      SERVICE wikibase:label { bd:serviceParam wikibase:language 'en, de, fr, it, cz, ru'. }
      {
        BIND(wd:", i, " as ?value) 
        ?item ?p ?value.
        ?item (wdt:P31/wdt:P279*) wd:Q11424.
        ?relation wikibase:directClaim ?p .
      }
    }")) %>% 
      mutate(id_film = str_extract(item, "Q.*$"),
             id_person = str_extract(value, "Q.*$")) #%>% 
    # rename("name_person" = "person_name", "name_film" = "itemLabel")
    
    return(filmography_query)
  }) #%>% select(id_person, name_person, id_film, name_film, "id_relation" = p, "name_relation" = relationLabel)
  
  # filmography_wfpp <- filmography_wfpp %>% select(id_person, name_person, id_film, name_film, "id_relation" = p, "name_relation" = relationLabel)
}

df_wfpp_wikidata_ids_for_mapping <- read_csv("data/wikidata/women-wfpp.csv") %>% 
  mutate(id_wiki = str_extract(item, "Q.*$")) %>% select(id_wiki) %>% pull(.)
filmography_wfpp <- get_filmography("wfpp", df_wfpp_wikidata_ids_for_mapping)
write_csv(filmography_wfpp, file = paste0("data/wikidata/wfpp-filmography-wikidata.csv"))


df_dff_wikidata_ids_for_mapping <- read_csv("data/ids/dff-woman-wikidata-cmprsd.csv") %>% select(wd_id) %>% distinct() %>% pull(.)
filmography_dff <- get_filmography("dff", df_dff_wikidata_ids_for_mapping)

filmography_dff_cleaned <- filmography_dff %>% 
  rename("name_person" = "valueLabel", "name_film" = "itemLabel", "id_relation" = p, "name_relation" = relationLabel) %>% 
  select(id_person, name_person, id_film, name_film, id_relation, name_relation)

write_csv(filmography_dff_cleaned, file = paste0("data/wikidata/dff-filmography-wikidata.csv"))


filmography_wfpp <- read.csv("data/wikidata/wfpp-filmography-wikidata.csv")

get_unique_films <- function(df_){
  filmography_unique_films <- df_ %>% 
    select(id_film, name_film) %>% distinct() %>% #View()
    mutate(film_description = tw_get_description(id = id_film ),
           film_publication_date = tw_get_p(id = id_film, p ="P577", only_first = TRUE ),
           film_director_id = tw_get_p(id = id_film, p ="P57", only_first = TRUE ),
           film_producer_id = tw_get_p(id = id_film, p ="P162", only_first = TRUE ),
           film_screenwriter_id = tw_get_p(id = id_film, p ="P58", only_first = TRUE),
           filmportal_id =  tw_get_p(id = id_film, p = "P2639"),
           imdb_id = tw_get_p(id = id_film, p = "P345"),# %>% as.character(.),
           viaf_id =  tw_get_p(id = id_film, p = "P214"),
           gnd_id =  tw_get_p(id = id_film, p = "P227"),
           dnb_id = tw_get_p(id = id_film, p = "P1292")
           ) %>% 
    unnest(film_description,film_publication_date,film_director_id,film_producer_id,film_screenwriter_id, filmportal_id, viaf_id, gnd_id, dnb_id ) %>% 
    mutate(film_director_name = tw_get_label(id = film_director_id),
           film_screenwriter_name = tw_get_label(id = film_screenwriter_id),
           film_producer_name = tw_get_label(id = film_producer_id),
           # imdb_id = str_c(imdb_id, sep = ",")
           imdb_id = map_chr(imdb_id, function(i){
             return <- paste(i, collapse = ",")}
             )
           ) 
}

dff_unique_film_ids <- filmography_dff_cleaned %>% select(id_film, name_film)# %>% head(10)
dff_unique_films <- get_unique_films(dff_unique_film_ids)
write_csv(dff_unique_films, file = "data/wikidata/dff_unique_films.csv")

wfpp_unique_film_ids <- filmography_wfpp %>% select(id_film, name_film)
wfpp_unique_films <- get_unique_films(wfpp_unique_film_ids)
save(filmography_unique_films, file = "data/wikidata/filmography_wfpp_unique_films.RData")
write_csv(filmography_unique_films, file = "data/wikidata/filmography_wfpp_unique_films.csv")

more_film_found <- anti_join(filmography_wfpp, filmography, by = c("id_film" = "id")) #%>% 

more_work_found <- anti_join(filmography, filmography_wfpp, by = c("id" = "id_film")) #%>% 
  select(id, label, description) %>% distinct()

filmography_wfpp %>% select(item) %>% distinct() %>% nrow()
filmography %>% select(id) %>% distinct() %>% nrow()

#######################
### erster Ansatz für WFPP-Frauen
#######################
## Bei diesem ersten Versuch war die Verbindung nach der gesucht wurde nicht Person-Film, sondern Person-Tätigkeit. 
## Die Abfrage lautete: welche Einträge gibt es, in der die Person folgende Tätigkeit (ausgehend von denen, die in der Biografie auf Wikidate genannt werden) ausführte 

## Die Abfragen liefern natürlich ähnliche Ergebnisse. Doch ist es wichtig zu bedenken, dass nur weil bei einer Frau steht, sie hätte eine Filmrolle übernommen, das nicht automatisch
## dazu führt, dass die entsprechenden Filme mit dieser Information verknüpft werden
## Mit der umgekehrten Frage, besteht das Problem gleichermaßen. Womöglich hat eine Frau an einem Film partizipiert, wird doch aber nicht genannt. 
## Der erste Ansatz ist darauf angewiesen, dass die jeweiligen Filme gut gepflegt werden und die Arbeit der Personen dort auch sichtbar wird. Das ist die analoge Perspektive zu den Sammlungen
## vom WFPP und DFF.


### ids für die Frauen via https://query.wikidata.org/#SELECT%20DISTINCT%20%3Fitem%20%3FitemLabel%20WHERE%20%7B%0A%20%20SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22.%20%7D%0A%20%20%7B%0A%20%20%20%20SELECT%20DISTINCT%20%3Fitem%20WHERE%20%7B%0A%20%20%20%20%20%20%3Fitem%20p%3AP7498%20%3Fstatement0.%20%20%20%20%20%20%0A%20%20%20%20%20%20%3Fstatement0%20%28ps%3AP7498%29%20_%3AanyValueP7498.%0A%20%20%20%20%7D%0A%20%20%7D%0A%7D
# abgefragt und im nachfolgenden Verzeichnis gespeichert:
df_wfpp_wikidata_ids <- read_csv("../data/wikidata/women-wfpp.csv") %>% 
  mutate(id_wiki = str_extract(item, "Q.*$"))

## occupation (https://www.wikidata.org/wiki/Property:P106) steht für die Tätigkeit der Person, zuerst die IDs für die jeweiligen Tätigkeiten holen, dann die label dafür

df_wfpp_occupation <- df_wfpp_wikidata_ids %>% 
  mutate(occupation = tw_get_p(id = id_wiki, p = "P106", only_first = FALSE)) %>% 
  unnest(occupation) %>% 
  # filter(!is.na(occupation)) %>% 
  mutate(label_occupation = tw_get_label(id = occupation),
         occupation_prop = tw_get_p(id = occupation, p = "P1687")) %>% # P1687 = wikidata-prop zum statement holen; problem: macht das ganze super generisch
  unnest(occupation_prop) %>%
  mutate(occupation_prop_label = tw_get_property_label(occupation_prop )) #%>% 
## die nachfolgenden zeilen dienten dazu, nur noch eine property-id für eine q-id zu behalten. das führt im späteren verlauf dazu, dass 4 Filme unter den Tisch fallen. Bei 5775 Filmen nicht viel, aber sie fehlen. Deswegen lasse ich doch alle properties drinnen, auch wenn das viel mehr Datenabfragen provoziert.
# mutate(occupation_prop_filter = row_number(), .by = c("id_wiki", "occupation", "label_occupation")) %>% 
# filter(occupation_prop_filter == 1)

## dataframe für eine freie Abfrage zusammenstellen. Die Anfrage lautet: gibt mir zu dieser Person in Spalte q alle Statements, die die Person und die occupation property p beinhalten

film_query <- df_wfpp_occupation %>% 
  select(occupation_prop, id_wiki) %>% 
  filter(!is.na(occupation_prop)) %>% 
  distinct() %>% 
  rename("p" = "occupation_prop", "q" = "id_wiki") %>% 
  as_tibble() %>% #head(1)
  mutate(id = row_number())

## künstlich erzeugte id, um darüber iterieren zu können
iter_film_query <- film_query %>% select(id) %>% pull(.)

## die hier genutzte Funktion aus dem wikidata-package hat ein issue mit curl: wenn ich versuche den ganzen df auf einmal abzufragen, wird intern versucht eine Variable zu überschreiben, was aber nicht funktioniert.
## deswegen die künstlichen ids, über die ich drüber mappe und den großen Datensatz filtere 
filmography <- map_df(iter_film_query, function(i){
  iter_df <- film_query %>% filter(id == i) %>% select(-id)
  films <- tw_query(query = iter_df, language = c("de", "en", "fr, it, cz, ru")) %>% 
    mutate(p = iter_df$p,
           q = iter_df$q)
  
})

# person_test_1 <- filmography %>% select(q, id) %>% distinct()
# person_test_2 <- filmography_test %>% select(q, id) %>% distinct()
# diff_persons_test <- person_test_1 %>% anti_join(., person_test_2)


# Ids holen zu veröffentlichungsdatum, director und screenwriter der oben erhaltenen Filme
film_info_ids <- filmography %>% 
  select(id) %>% 
  distinct() %>% 
  mutate(film_title = tw_get_p(id = id, p ="P1476"),
         film_publication_date = tw_get_p(id = id, p ="P577", only_first = TRUE ),
         film_director_id = tw_get_p(id = id, p ="P57", only_first = TRUE ),
         film_screenwriter_id = tw_get_p(id = id, p ="P58", only_first = TRUE ),
         film_producer_id = tw_get_p(id = id, p ="P162", only_first = TRUE ),
         filmportal_id =  tw_get_p(id = id, p = "P2639"),
         imdb_id = tw_get_p(id = id, p = "P345") %>% as.character(.),
         viaf_id =  tw_get_p(id = id, p = "P214"),
         gnd_id =  tw_get_p(id = id, p = "P227"),
         dnb_id = tw_get_p(id = id, p = "P1292")) #%>% 
# unnest()

# und die label zu den Ids. Arbeitsschritt getrennt, weil die Abfrage super lang dauert.
film_info <- film_info_ids %>% 
  mutate(film_director_name = tw_get_label(id = film_director_id),
         film_screenwriter_name = tw_get_label(id = film_screenwriter_id),
         film_producer_name = tw_get_label(id = film_producer_id))

film_info <- film_info %>% # just some column sorting and clearifing names
  select("film_id" = "id", film_publication_date, film_director_name, film_director_id, film_screenwriter_name, film_screenwriter_id, film_producer_name, film_producer_id, filmportal_id, imdb_id, viaf_id, gnd_id, dnb_id)

df_wfpp_wikidata_occupation <- df_wfpp_occupation %>% 
  select("person_name" = "itemLabel", "person_id" = "id_wiki", "person_link" = "item", occupation_prop_label, occupation_prop) %>% distinct()

wfpp_work_wikidata <- filmography %>% 
  left_join(., df_wfpp_wikidata_occupation, by = c("q" = "person_id", "p" = "occupation_prop")) %>% #View()
  rename("film_id" = "id", "film_title" = "label", "film_description" = "description") %>% 
  left_join(., film_info, by = "film_id") %>% 
  distinct() %>% 
  rename("occupation_prop" = "p", "person_id" = "q")

save(wfpp_work_wikidata, file = "data/wikidata/wfpp-work-wikidata.RData")
write_csv(wfpp_work_wikidata, file = "data/wikidata/wfpp-work-wikidata.csv")





