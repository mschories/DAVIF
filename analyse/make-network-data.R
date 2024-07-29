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
      size = between_
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


filmography_wfpp <- read.csv("data/wikidata/wfpp-filmography-wikidata.csv")

wfpp_connected_women <- get_connected_women(filmography_wfpp)

# wfpp_wikidata_edges <- filmography_wfpp %>% rename("from" = "id_film", "to" = "id_person")

wfpp_connected_women_nodes <- filmography_wfpp %>% 
  select(id_person, name_person) %>% distinct() 

graph_data_connected_women_wfpp <- make_graph_data(wfpp_connected_women, wfpp_connected_women_nodes)

save(graph_data_connected_women_wfpp, file = "data/doc/graph_data_connected_women_wfpp.RData")


# DFF - Daten ------------------------------

df_dff_woman <- read_tsv("data/DataViz/DatenDFF/UP1-1945.tsv") %>% 
  filter(Geschlecht != "M") %>% 
  mutate(year = str_extract(Ordnungsdatum, "^\\d{4,}")) %>% 
  select("id_person" = "Person", "id_film" = "Filmwerk", year)

df_dff_connected_woman <- get_connected_women(df_dff_woman) 



dff_connected_woman_nodes <- read_tsv("data/DataViz/DatenDFF/UP1-1945.tsv") %>% 
  filter(Geschlecht != "M") %>% select("id_person" = "Person", "name_person" = "IDName") %>% distinct()

graph_data_connected_women_dff <- make_graph_data(df_dff_connected_woman, dff_connected_woman_nodes)
save(graph_data_connected_women_dff, file = "data/doc/graph_data_connected_women_dff.RData")


graph_data_connected_women_dff %>% activate(nodes) %>% as_tibble() %>% View()


# Gemeinsames Netzwerk ----------------------------------
df_dff_connected_woman_year <- get_connected_women_year(df_dff_woman) 
save(df_dff_connected_woman_year, file = "data/doc/df_dff_connected_woman_year.RData")



