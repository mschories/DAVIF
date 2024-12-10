library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggiraph)
library(visNetwork)
library(MetBrewer)


# netzwerk rolle - film - wfpp ----------------------------

load(file = "data/doc/graph_wfpp_rollen_filme.RData")

colors_roles_wfpp <- wfpp_rollen_filme %>% activate(nodes) %>% as_tibble() %>% select(role) %>% distinct() %>% 
  arrange(role) %>% 
  mutate(values = rainbow(length(role)),
         values = ifelse(is.na(role), "#000000", values)) %>% 
  deframe()

set.seed(0401)


ggraph(wfpp_rollen_filme, layout = 'stress') + 
  geom_edge_link(color = "#bbbbbb") + 
  geom_node_text(aes(label = title), repel = TRUE, max.overlaps = 35, lineheight = 0.7, vjust = 1) +
  geom_node_point(aes(colour = color)) +
  scale_color_manual(values = colors_roles_wfpp, name = "Tätigkeiten")

# netzwerk rolle - film - wd ----------------------------

load(file = "data/doc/graph_wd_rollen_filme.RData")

colors_roles_wd <- wd_rollen_filme %>% activate(nodes) %>% as_tibble() %>% select(role) %>% distinct() %>% 
  arrange(role) %>% 
  mutate(values = rainbow(length(role)),
         values = ifelse(is.na(role), "#000000", values)) %>% 
  deframe()


ggraph(wd_rollen_filme, layout = 'stress') + 
  geom_edge_link(color = "#bbbbbb") + 
  geom_node_point(aes(colour = color)) +
  geom_node_text(aes(label = title), repel = TRUE, max.overlaps = 35, lineheight = 0.7, vjust = 1) +
  scale_color_manual(values = colors_roles_wd, name = "Tätigkeiten")


# netzwerk rolle - film - dff ----------------------------

load(file = "../data/doc/graph_dff_rollen_filme.RData")

colors_roles_dff <- dff_rollen_filme %>% activate(nodes) %>% as_tibble() %>% select(role) %>% distinct() %>% 
  arrange(role) %>% 
  mutate(values = rainbow(length(role)),
         values = ifelse(is.na(role), "#000000", values)) %>% 
  deframe()


ggraph(dff_rollen_filme, layout = 'stress') + 
  geom_edge_link(color = "#bbbbbb") + 
  geom_node_point(aes(colour = color)) +
  geom_node_text(aes(label = title), repel = TRUE, max.overlaps = 35, lineheight = 0.7, vjust = 1) +
  scale_color_manual(values = colors_roles_dff, name = "Tätigkeiten")
