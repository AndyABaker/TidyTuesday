library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(lubridate)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)


tuesdata <- tidytuesdayR::tt_load(2021, week = 25)
tweets <- tuesdata$tweets


other_words <- data.frame(word = as.character(c(1:2000, "t.co", "https")))

tweets_tidy <- tweets %>% 
  unnest_tokens(word, content) %>% 
  anti_join(stop_words) %>% 
  anti_join(other_words)

word_cooccurences <- tweets_tidy %>% 
  pairwise_count(word, datetime, sort = TRUE)
word_cooccurences

set.seed(987)
word_cooccurences %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_colour = n, edge_width = n)) +
  #geom_node_point(size = 5, color = "darkslategray4") +
  geom_node_label(aes(label = name), size = 10) + 
  scale_edge_colour_viridis() + 
  scale_edge_width(range = c(0.5, 5)) + 
  labs(
    title = "Words commonly found together in #DuBoisChallenge tweets",
    caption = "Source: #DuBoisChallenge tweets | Visualisation: @Andy_A_Baker"
    ) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", family = "Rubik", 
                                  size = 50, hjust = 0.5),
        plot.caption = element_text(colour = "black", family = "Rubik", 
                                    size = 35),
        plot.margin = margin(10, 10, 10, 10))

ggsave(dpi = 300, width = 10, height = 10, units = "in", 
       filename = "2021_week25_webduboisandjuneteenth.jpeg", device = "jpeg")
