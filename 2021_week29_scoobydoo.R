library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(Cairo)
library(tidytext)
library(igraph)
library(ggraph)

tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

# Excellent resource here: https://chryswoods.com/text_analysis_r/
bigram_counts <- scoobydoo %>% 
  mutate(text = title) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(1, "mm")), 
                 end_cap = circle(4, "mm"),
                 start_cap = circle(6, "mm")) + 
  geom_node_text(aes(label = name), colour = "#7A51A1", size = 10) +
  theme_void() + 
  labs(title = "Common words in Scooby Doo episode/film titles",
       caption = "Source: Kaggle | Visualisation: @Andy_A_Baker") + 
  theme(panel.background = element_rect(fill = "#FAFCEE", colour = NA),
        plot.background = element_rect(fill = "#FAFCEE", colour = NA),
        text = element_text(family = "Rubik"),
        plot.caption = element_text(family = "Rubik", face = "bold", 
                                    size = 35, colour = "#BDD73B"),
        plot.title = element_text(family = "Rubik", face = "bold", 
                                  size = 70, hjust = 0.5, colour = "#BDD73B"),
        plot.margin = margin(10, 10, 10, 10))

ggsave(dpi = 300, width = 10, height = 10, units = "in", type = "cairo",
       filename = "2021_week29_scoobydoo.jpeg", 
       device = "jpeg")
