library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggiraph)
library(visNetwork)
library(MetBrewer)

make_graph_data <- function(df_, node_info){
  graph_data <- df_ %>% 
    as_tbl_graph(., directed = F) %>% 
    activate(nodes) %>% #as_tibble() %>% View()
    left_join(., node_info, by = c("name" = "id_person"), copy = T) %>%
    mutate(
      degree = igraph::degree(.),
      # indegree = igraph::degree(., mode = "in"),
      # outdegree = igraph::degree(., mode = "out"),
      strength = igraph::strength(.),
      between_ = igraph::betweenness(.),
      close = igraph::closeness(.),
      title = name_person,
      value = between_,
      size = 1
      # group = group_louvain()
    ) #%>% 
  return(graph_data)
}

make_role_graph_data <- function(df_, node_info){
  graph_data <- df_ %>% 
    as_tbl_graph(., directed = T) %>% 
    activate(nodes) %>% #as_tibble() %>% View()
    left_join(., node_info, by = c("name" = "id_role"), copy = T) %>%
    mutate(
      degree = igraph::degree(.),
      # indegree = igraph::degree(., mode = "in"),
      # outdegree = igraph::degree(., mode = "out"),
      strength = igraph::strength(.),
      between_ = igraph::betweenness(.),
      close = igraph::closeness(.),
      title = paste0(role,"\n",name_person),
      title = ifelse(is.na(role), "", title),
      color = role,
      color = ifelse(is.na(color), "film", color),
      value = between_,
      size = 1
      
      # group = group_louvain()
    ) #%>% 
  return(graph_data)
}

get_connected_women <- function(df_){
  df_ %>% 
    select(id_person, id_film) %>% 
    distinct() %>% 
    mutate(group = cur_group_id(),
           group_members = n(), .by = id_film) %>% 
    select(id_person, group) %>% 
    group_by(group) %>% 
    expand(id_person, id_person, .name_repair = "unique") %>% 
    ungroup() %>% 
    filter(`id_person...1` != `id_person...2`) %>% 
    rowwise() %>%                                
    mutate(revCheck = str_c(str_sort(c(`id_person...1`, `id_person...2`)), collapse = "")) %>% 
    group_by(group, revCheck) %>% 
    slice(1) %>% 
    ungroup()%>%
    rename("from" = "id_person...1", "to" = "id_person...2")
}

get_connected_women_year <- function(df_){
  df_ %>% 
    select(id_person, id_film, year) %>% 
    distinct() %>% 
    mutate(group = cur_group_id(),
           group_members = n(), .by = c(id_film, year)) %>% 
    select(id_person, group,year) %>% 
    group_by(group,year) %>% 
    expand(id_person, id_person, .name_repair = "unique") %>% 
    ungroup() %>% 
    filter(`id_person...1` != `id_person...2`) %>% 
    rowwise() %>%                                
    mutate(revCheck = str_c(str_sort(c(`id_person...1`, `id_person...2`)), collapse = "")) %>% 
    group_by(group, year, revCheck) %>% 
    slice(1) %>% 
    ungroup()%>%
    rename("from" = "id_person...1", "to" = "id_person...2")
}



# WFPP - Daten --------------------


load(file = "data/doc/df_merged_bio_film.RData")
filmography_wfpp <- read_csv("data/doc/wfpp-filmography-ids.csv", show_col_types = FALSE) %>% 
  filter(id %in% df_merged_bio_film$id) %>% 
  select(id_person, name, title, role) %>%
  mutate(title = trimws(title),
    id_film = cur_group_id(), .by= title) %>% 
  distinct()

filmography_wfpp %>% select(name) %>% distinct() %>% nrow()

wfpp_connected_women <- get_connected_women(filmography_wfpp)

# wfpp_wikidata_edges <- filmography_wfpp %>% rename("from" = "id_film", "to" = "id_person")

wfpp_connected_women_nodes <- filmography_wfpp %>% 
  select(id_person, name_person = name) %>% distinct() %>% 
  mutate(id_person = as.character(id_person))

graph_data_connected_women_wfpp <- make_graph_data(wfpp_connected_women, wfpp_connected_women_nodes)

graph_data_connected_women_wfpp %>% 
  # as_tbl_graph(., directed = F) %>% 
  activate(nodes) %>% as_tibble() %>% View()

save(graph_data_connected_women_wfpp, file = "data/doc/graph_data_connected_women_wfpp.RData")


## wfpp rollen - filme ------------------

wfpp_graph_rollen_filme <- filmography_wfpp %>% 
  mutate(id_role = paste0(id_person, "-", role)) %>% 
  select(from = id_role, to = id_film)

wfpp_node_rollen_filme <- filmography_wfpp %>% 
  mutate(id_role = paste0(id_person, "-", role)) %>% 
  select(id_role, name_person = name, role) %>% 
  distinct()


wfpp_rollen_filme <- make_role_graph_data(wfpp_graph_rollen_filme, wfpp_node_rollen_filme)

# wfpp_node_info_rollen_filme <- filmography_wfpp %>% 
save(wfpp_rollen_filme, file = "data/doc/graph_wfpp_rollen_filme.RData")

wfpp_rollen_filme %>% activate(nodes) %>% as_tibble() %>% View()

# Wikidata - Daten --------------------


# load(file = "data/doc/df_merged_bio_film.RData")
filmography_wd <- read.csv("data/wikidata/wfpp-filmography-wikidata.csv") %>% 
  filter(id_person %in% df_merged_bio_film$id_person)

wd_connected_women <- get_connected_women(filmography_wd)

# wfpp_wikidata_edges <- filmography_wfpp %>% rename("from" = "id_film", "to" = "id_person")

wd_connected_women_nodes <- filmography_wd %>% 
  select(id_person, name_person) %>% distinct() 

graph_data_connected_women_wd <- make_graph_data(wd_connected_women, wd_connected_women_nodes)

save(graph_data_connected_women_wd, file = "data/doc/graph_data_connected_women_wd.RData")

## wd rollen - filme -------------------

wd_graph_rollen_filme <- filmography_wd %>% 
  mutate(id_role = paste0(id_person, "-", name_relation)) %>% 
  select(from = id_role, to = id_film)

wd_node_rollen_filme <- filmography_wd %>% 
  mutate(id_role = paste0(id_person, "-", name_relation)) %>% 
  select(id_role, name_person, role = name_relation) %>% 
  distinct()


wd_rollen_filme <- make_role_graph_data(wd_graph_rollen_filme, wd_node_rollen_filme)
save(wd_rollen_filme, file = "data/doc/graph_wd_rollen_filme.RData")


# DFF - Daten ------------------------------

df_dff_woman <- read_csv(file = "data/dff/df_dff_person_occupation.csv", show_col_types = F) %>% 
  filter(uid_2 %in% df_merged_bio_film$filmportal_id) %>% #View()
  # filter(Geschlecht != "M") %>% 
  # mutate(year = str_extract(Ordnungsdatum, "^\\d{4,}")) %>% 
  select("id_person" = "uid_2", "id_film" = "uid")

df_dff_connected_woman <- get_connected_women(df_dff_woman) 



dff_connected_woman_nodes <- read_csv(file = "data/dff/df_dff_person_occupation.csv", show_col_types = F) %>% 
  filter(uid_2 %in% df_merged_bio_film$filmportal_id) %>% select("id_person" = "uid_2", "name_person" = "idname") %>% distinct()

graph_data_connected_women_dff <- make_graph_data(df_dff_connected_woman, dff_connected_woman_nodes)
save(graph_data_connected_women_dff, file = "data/doc/graph_data_connected_women_dff.RData")


graph_data_connected_women_dff %>% activate(nodes) %>% as_tibble() %>% View()

## dff rollen - filme -------------------

dff_filmography <- read_csv(file = "data/dff/df_dff_person_occupation.csv", show_col_types = F) %>% 
  filter(uid_2 %in% df_merged_bio_film$filmportal_id) %>% #View()
  mutate(id_role = paste0(uid_2, "-", rel))


dff_graph_rollen_filme <- dff_filmography %>% 
  select(from = id_role, to = uid)

dff_node_rollen_filme <- dff_filmography %>% 
  select(id_role, name_person = idname, role = rel) %>% 
  distinct()


dff_rollen_filme <- make_role_graph_data(dff_graph_rollen_filme, dff_node_rollen_filme)
save(dff_rollen_filme, file = "data/doc/graph_dff_rollen_filme.RData")




# Gemeinsames Netzwerk ----------------------------------
df_dff_connected_woman_year <- get_connected_women_year(df_dff_woman) 
save(df_dff_connected_woman_year, file = "data/doc/df_dff_connected_woman_year.RData")



