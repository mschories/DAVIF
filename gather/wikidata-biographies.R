library(tidyverse)
library(tidywikidatar)
tw_enable_cache()
tw_set_cache_folder(path = fs::path(fs::path_home_r(), "R", "tw_data"))
tw_set_language(language = "en")
tw_create_cache_folder(ask = FALSE)


get_bio_data <- function(df_ids){
  bio_wikidata_ids <- df_ids %>% 
  mutate(discription = tw_get_description(id = id_person),
         sex_or_gender_id = tw_get_p(id = id_person, p = "P21") %>% as.character(.),
         citizenship_id = tw_get_p(id = id_person, p = "P27"),
         date_of_birth = tw_get_p(id = id_person, p = "P569", only_first = TRUE) %>% as.character(.),
         place_of_birth_id = tw_get_p(id = id_person, p = "P19", only_first = TRUE),
         date_of_death = tw_get_p(id = id_person, p = "P570", only_first = TRUE) %>% as.character(.),
         place_of_death_id = tw_get_p(id = id_person, p = "P20", only_first = TRUE),
         ethnic_group_id = tw_get_p(id = id_person, p = "P172") %>% as.character(.),
         sexual_orientation_id = tw_get_p(id = id_person, p = "P91") %>% as.character(.), 
         wfpp_id = tw_get_p(id = id_person, p = "P7498"), 
         imdb_id = tw_get_p(id = id_person, p = "P345") %>% as.character(.),
         viaf_id =  tw_get_p(id = id_person, p = "P214"),
         gnd_id =  tw_get_p(id = id_person, p = "P227"),
         dnb_id = tw_get_p(id = id_person, p = "P1292"),
         filmportal_id =  tw_get_p(id = id_person, p = "P2639")
  ) %>% unnest_auto(col = c(citizenship_id)) %>% unnest_auto(col = c(place_of_birth_id)) %>% unnest_auto(col = c(place_of_death_id)) %>% unnest_auto(col = c(wfpp_id)) %>% unnest_auto(col = c(viaf_id)) %>% unnest_auto(col = c(gnd_id)) %>% unnest_auto(col = c(dnb_id))%>% unnest_auto(col = c(filmportal_id))
# unnest_longer(col = c(sex_or_gender, citizenship, date_of_birth, place_of_birth, date_of_death, ethnic_group, sexual_orientation, wfpp_id, imdb_id), keep_empty = TRUE)

  df_return_bio <- bio_wikidata_ids %>% 
    mutate(
      sex_or_gender = tw_get_label(id = sex_or_gender_id),
      citizenship = tw_get_label(id = citizenship_id),
      place_of_birth = tw_get_label(id = place_of_birth_id),
      place_of_death = tw_get_label(id = place_of_death_id),
      ethnic_group = tw_get_label(id = ethnic_group_id),
      sexual_orientation = tw_get_label(id = sexual_orientation_id)
    ) %>% 
    select("name_person" = "itemLabel", id_person, "description" = "discription", sex_or_gender, citizenship, date_of_birth, date_of_death, place_of_birth, place_of_death, ethnic_group, sexual_orientation, wfpp_id, imdb_id, viaf_id, gnd_id, dnb_id, filmportal_id)
  
  return(df_return_bio)
}

# (wfpp) get biografies from wikidata for wfpp women --------------------------

df_wfpp_wikidata_ids <- read_csv("../data/wikidata/women-wfpp.csv") %>% 
  mutate(id_person = str_extract(item, "Q.*$"))
df_wfpp_bio_wikidata <- get_bio_data(df_wfpp_wikidata_ids)

write_csv(df_wfpp_bio_wikidata, file = "data/wikidata/wfpp-bio-wikidata.csv")


# (wfpp)  cleaning data wfpp - one woman per line -------------------------------

df_wfpp_online_long <- read_csv(file = "data/wikidata/wfpp-bio-wikidata.csv", show_col_types = FALSE) 

df_wfpp_bio_wikidata_cmprsd_ctznshp <- df_wfpp_online_long %>% 
  summarise(citizenship_cmprsd = paste(citizenship, collapse = "|"), .by = c(name_person, id_person))

df_wfpp_online_cmprsd_viaf <- df_wfpp_online_long %>% 
  summarise(online_viaf_id = paste(viaf_id, collapse = "|"), .by = c(id_person, name_person))

df_wfpp_online_cmprsd_gnd <- df_wfpp_online_long %>% 
  summarise(online_gnd_id = paste(gnd_id, collapse = "|"), .by = c(id_person, name_person))

df_wfpp_online_cmprsd_online_wfpp <- df_wfpp_online_long %>% 
  select(name_person, id_person, wfpp_id) %>% distinct() %>% 
  summarise(online_wfpp_id = paste(wfpp_id, collapse = "|"), .by = c(id_person, name_person))

df_wfpp_bio_wikidata_cmprsd <- df_wfpp_online_long %>% 
  left_join(., df_wfpp_bio_wikidata_cmprsd_ctznshp) %>% 
  left_join(., df_wfpp_online_cmprsd_viaf) %>% 
  left_join(., df_wfpp_online_cmprsd_gnd) %>% 
  left_join(., df_wfpp_online_cmprsd_online_wfpp) %>% 
  select(name_person, id_person, description, sex_or_gender, citizenship_cmprsd, everything(), -citizenship, -viaf_id, -gnd_id, -wfpp_id) %>% 
  distinct()

write_csv(df_wfpp_bio_wikidata_cmprsd, file = "data/wikidata/wfpp-bio-wikidata-cmprsd.csv")

# (wfpp) make id-csv for merging with all id info ---------------------------------------------

# rm(df_wfpp_bio_wikidata_ids_test)
df_wfpp_bio_wikidata_ids <- df_wfpp_bio_wikidata_cmprsd %>% 
  # rename_with(~paste0("online_", ., recycle0 = TRUE)) %>% View()
  select(online_wfpp_name = name_person, online_wd_id = id_person, online_wfpp_id, online_imdb_id = imdb_id, online_viaf_id, online_gnd_id, online_filmportal_id = filmportal_id) %>% 
  distinct()

write_csv(df_wfpp_bio_wikidata_ids, file = "data/ids/wfpp-woman-wikidata.csv")

# (dff) get biografies from wikidata for dff women ----------------------------


df_dff_pioniers_ <- read_csv("data/dff/dff-person-bio.csv") %>%
  select(filmportal_id, name) %>% 
  distinct()

df_dff_wikidata <- read_csv("data/wikidata/women-filmportal.csv", show_col_types = FALSE) %>% 
  as_tibble() %>% 
  mutate(value = toupper(value),
         id_person = str_extract(item, "Q.*$")) %>% 
  filter(value %in% df_dff_pioniers_$filmportal_person_id)

df_dff_bio_wikidata_ <- get_bio_data(df_dff_wikidata)

# (dff) cleaning data - one woman per line -------------------------------------

df_dff_bio_wikidata_cmprsd_ctznshp <- df_dff_bio_wikidata_ %>% 
  summarise(citizenship_cmprsd = paste(citizenship, collapse = "|"), .by = c(name_person, id_person))

df_dff_bio_wikidata_cmprsd <- df_dff_bio_wikidata_ %>% 
  select(-citizenship) %>% 
  left_join(., df_dff_bio_wikidata_cmprsd_ctznshp, by = join_by(name_person, id_person)) %>% 
  distinct()

df_dff_bio_wikidata_cmprsd_viaf <- df_dff_bio_wikidata_ %>% 
  summarise(viaf_cmprsd = paste(viaf_id, collapse = "|"), .by = c(name_person, id_person))

df_dff_bio_wikidata_cmprsd <- df_dff_bio_wikidata_cmprsd %>% 
  select(-viaf_id) %>% 
  left_join(., df_dff_bio_wikidata_cmprsd_viaf, by = join_by(name_person, id_person)) %>% 
  distinct()

df_dff_bio_wikidata_cmprsd_gnd <- df_dff_bio_wikidata_ %>% 
  summarise(gnd_cmprsd = paste(gnd_id, collapse = "|"), .by = c(name_person, id_person))

df_dff_bio_wikidata_cmprsd <- df_dff_bio_wikidata_cmprsd %>% 
  select(-gnd_id) %>% 
  left_join(., df_dff_bio_wikidata_cmprsd_gnd, by = join_by(name_person, id_person)) %>% 
  distinct() %>% as_tibble()

write.csv(df_dff_bio_wikidata_cmprsd, file = "data/wikidata/dff-bio-wikidata.csv")



# (dff)  make id-csv for merging with all id info  ------------------------------
df_dff_bio_wikidata_cmprsd_ids <- df_dff_bio_wikidata_cmprsd %>% 
  select("wd_id" = "id_person", wfpp_id, imdb_id, viaf_id, gnd_id, filmportal_id) %>% distinct()

write_csv(df_dff_bio_wikidata_cmprsd_ids, file = "data/ids/dff-woman-wikidata.csv")

df_dff_bio_wikidata_cmprsd_ids <- df_dff_bio_wikidata_cmprsd %>% 
  select(name = name_person, "wd_id" = id_person, wfpp_id, imdb_id, viaf_id = viaf_cmprsd, gnd_id = gnd_cmprsd, filmportal_id)

write_csv(df_dff_bio_wikidata_cmprsd_ids, file = "data/ids/dff-woman-wikidata-cmprsd.csv")



