library(tidyverse)
library(lubridate)
library(igraph)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

nf <- netflix_titles %>% 
  mutate(date_added = mdy(date_added),
         type = factor(type),
         show_id = factor(show_id)
  ) %>% 
  select(show_id, title, type, cast) %>% 
  drop_na() %>% 
  mutate(cast = strsplit(cast, ", ")) %>% 
  unnest(cols = cast) %>% 
  distinct() %>% 
  select(show_id, cast)

nf_c <- nf %>% 
  group_by(show_id) %>% 
  left_join(nf, by = c("show_id")) %>% 
  filter(cast.x != cast.y) %>% 
  mutate(value = 1) %>% 
  group_by(cast.x, cast.y) %>% 
  summarise(n_cast = sum(value)) %>% 
  filter(n_cast > 3)


p <- graph_from_data_frame(nf_c, directed = FALSE)

V(p)$size <- log(strength(p)) * 2
V(p)$label <- ifelse(strength(p) >= 8, V(p)$name, NA)

set.seed(30)


png("2021_week17_netflixtitles.png", units = "in", width = 10, height = 10, res = 300)
par(mar = c(1, 1, 1, 1))
plot(p,
     vertex.color = "#2a9d8f", 
     vertex.label.color = "#13242A", 
     vertex.label.cex = 0.5, 
     edge.color = "#9EE5DD",
     layout = layout_with_fr)
title("Netflix cast network", cex.main = 1, col.main = "#13242A")
dev.off()
