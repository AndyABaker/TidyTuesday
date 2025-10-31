library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(Cairo)
library(tidygraph)
library(ggraph)
library(ggnetwork)
library(intergraph)
library(ggtext)
library(countrycode)
library(ggflags)
library(lubridate)


tuesdata <- tidytuesdayR::tt_load(2021, week = 28)
holidays <- tuesdata$holidays %>% 
  mutate(country_code = tolower(countrycode(country, 
                                            origin = "country.name", 
                                            destination = "iso2c")),
         independence_from = str_remove_all(independence_from, "\\[[0-9]+?\\]") %>% 
           str_wrap(., 20),
         independence_from = ifelse(str_detect(independence_from, "^Allied.+"),
                                    "Allied occupying powers",
                                    independence_from)) %>% 
  rename(from = country, 
         to = independence_from) %>% 
  filter(!is.na(to))


net <- as_tbl_graph(holidays, directed = TRUE) %>% 
  activate(edges) %>% 
  select(from, to) %>% 
  activate(nodes)


network <- ggnetwork(asNetwork(net))

network$country <- tolower(countrycode(as.character(network$vertex.names),
                                       "country.name","iso2c"))

network <- left_join(network, holidays, 
                     by = c("country" = "country_code"))

set.seed(1234)
network %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend,
             country = country)) + 
  geom_edges() + 
  geom_flag(size = 10) + 
  geom_text(data = subset(network, is.na(country) & vertex.names != "NA"),
                          aes(label = vertex.names),
            size = 6) + 
  theme_void()

ggsave(dpi = 300, width = 12, height = 12, units = "in", type = "cairo",
       filename = "2021_week28_internationalindependencedays.jpeg", 
       device = "jpeg")
