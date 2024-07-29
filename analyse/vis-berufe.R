library(tidyverse)
library(ggforce)
library(ggiraph)

### liste frauen in beiden datensätzen
### zunächst auf basis von wikidata

load("data/doc/df_merged_all.RData")

women_occupation <- df_merged %>% 
  filter(!is.na(Titel)) #%>% 
  
wfpp_max_occupations <- women_occupation %>% 
  mutate(splitted = str_split(worked_as, "[|]") %>% lengths(.)) %>% #View()
  arrange(desc(splitted)) %>% head(1) %>% pull(.)

wfpp_occupations <- women_occupation %>% 
  select(id, Person, name, worked_as) %>% 
  distinct() %>% 
  separate(col = worked_as, sep = "[|]", into = str_c("worked_as_", 1:wfpp_max_occupations)) %>% #View()
  pivot_longer(cols = starts_with("worked_as_"), names_to = "names", values_to = "wfpp_occupation") %>% 
  filter(!is.na(wfpp_occupation)) %>% 
  mutate(plot_y = row_number(), .by = name,
         counted = 1)

dff_occupations <- women_occupation %>% 
  select(id, Person, name, Rel) %>% 
  # distinct() %>% 
  reframe(counted = n(), .by = c("name", "Rel"))  %>% 
  mutate(plot_y = row_number(), .by = name)

### wie viele berufsbezeichnungen gibt es für die jeweiligen frauen in den jeweiligen sammlungen
### lässt sich das auf einen kreis zeichnen, so dass formen noch nicht überlappen?

max_wfpp <- wfpp_occupations %>% reframe(counted = n(), .by = name) %>% arrange(desc(counted)) %>% select(counted) %>% head(1) %>% pull(.)#%>% View()
max_dff <- dff_occupations %>% reframe(counted = n(), .by = name) %>% arrange(desc(counted))  %>% select(counted) %>% head(1) %>% pull(.)#%>% View()

max_x_breaks <- ifelse(max_wfpp > max_dff, max_wfpp, max_dff)

ggplot() +
  geom_point(data = wfpp_occupations, aes(x= plot_y, y = 0), color = "pink") +
  geom_point(data = wfpp_occupations, aes(x= plot_y, y = 1, size = counted), color = "cornflowerblue") +
  geom_point(data = dff_occupations, aes(x= plot_y, y = 2, size = counted), color = "mediumseagreen") +
  scale_y_continuous(limits = c(0,2.5), expand = c(0, NA), breaks = c(1,2), minor_breaks = c(0.5,1.5,2.5)) +
  scale_x_continuous(breaks = c(0:max_x_breaks)) +
  scale_size_area() +
  facet_wrap(~name, ncol = 9) +
  coord_polar() +
  theme_void() + ffg_facets_polar

plot <- ggplot() +
  geom_point(data = wfpp_occupations, aes(x= plot_y, y = 0), color = "pink") +
  geom_point_interactive(data = wfpp_occupations, aes(x= plot_y, y = 1, size = counted, tooltip = wfpp_occupation), color = "cornflowerblue") +
  geom_point_interactive(data = dff_occupations, aes(x= plot_y, y = 2, size = counted, tooltip = paste0(Rel, ": ", counted)), color = "mediumseagreen", alpha = .7) +
  scale_y_continuous(limits = c(0,2.5), expand = c(0, NA), breaks = c(1,2), minor_breaks = c(0.5,1.5,2.5)) +
  scale_x_continuous(breaks = c(0:max_x_breaks)) +
  scale_size_area() +
  facet_wrap(~name, ncol = 6) +
  coord_polar() +
  theme_void() + ffg_facets_polar + theme(legend.position = "none")

# systemfonts::register_font("GTAmericaMonoLC-Rg",plain = "GTAmericaMonoLC-Rg")
# gdtools::register_gfont("Roboto Mono")
girafe(ggobj = plot)
