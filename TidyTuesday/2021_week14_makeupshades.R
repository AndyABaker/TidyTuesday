library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(data.table)
library(ggbeeswarm)
library(ggrepel)

allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')

d_split <- allCategories %>% 
  mutate(gem = ifelse(categories %like% "gem", 1, 0),
         textile = ifelse(categories %like% "textile", 1, 0),
         misc = ifelse(categories %like% "misc", 1, 0),
         color = ifelse(categories %like% "color", 1, 0),
         rock = ifelse(categories %like% "rock", 1, 0),
         food = ifelse(categories %like% "food", 1, 0),
         wood = ifelse(categories %like% "wood", 1, 0),
         plant = ifelse(categories %like% "plant", 1, 0),
         animal = ifelse(categories %like% "animal", 1, 0),
         skin = ifelse(categories %like% "skin", 1, 0),
         drink = ifelse(categories %like% "drink", 1, 0),
         location = ifelse(categories %like% "location", 1, 0),
         descriptor = ifelse(categories %like% "descriptor", 1, 0),
         compliment = ifelse(categories %like% "compliment", 1, 0),
         metal = ifelse(categories %like% "metal", 1, 0),
         sand = ifelse(categories %like% "sand", 1, 0),
         name_c = ifelse(categories %like% "name", 1, 0)) %>% 
  pivot_longer(cols = gem:name_c, names_to = "category") %>% 
  filter(value != 0) %>% 
  select(-value) %>% 
  mutate(category = factor(case_when(category == "gem" ~ "Gem",
                                     category == "textile" ~ "Texture",
                                     category == "misc" ~ "Misc",
                                     category == "color" ~ "Colour",
                                     category == "rock" ~ "Rock",
                                     category == "food" ~ "Food",
                                     category == "wood" ~ "Wood",
                                     category == "plant" ~ "Plant",
                                     category == "animal" ~ "Animal",
                                     category == "skin" ~ "Skin",
                                     category == "drink" ~ "Drink",
                                     category == "location" ~ "Location",
                                     category == "descriptor" ~ "Descriptor",
                                     category == "compliment" ~ "Compliment",
                                     category == "metal" ~ "Metal",
                                     category == "sand" ~ "Sand",
                                     category == "name_c" ~ "Name"))) %>% 
  group_by(category) %>% 
  mutate(n = n()) %>% 
  filter(n > 150) %>% 
  mutate(lightness_mean = mean(lightness), wt = lightness)

d_split %>% 
  ggplot(aes(reorder(category, lightness_mean), lightness, colour = hex, label = name)) + 
  geom_quasirandom() + 
  geom_text_repel(data = top_n(d_split, 5, wt = lightness), nudge_y = 0.05, nudge_x = 0.3, segment.alpha = 0.1, family = "Rubik", size = 7, colour = "black") + 
  geom_text_repel(data = top_n(d_split, -5, wt = lightness), nudge_y = -0.05, nudge_x = 0.3, segment.alpha = 0.1, family = "Rubik", size = 7, colour = "black") + 
  scale_colour_identity() + 
  labs(caption = "Source: The Pudding data | Visualisation: @Andy_A_Baker",
       x = NULL,
       title = "Bias in foundation shade names") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#E5E5E3"),
        plot.background = element_rect(fill = "#E5E5E3"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Rubik", face = "bold", colour = "black", size = 100),
        axis.text = element_text(family = "Rubik", face = "bold", colour = "black", size = 50),
        plot.caption = element_text(family = "Rubik", size = 30),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  coord_flip()


ggsave(dpi = 300, width = 10, height = 12, units = "in", filename = "2021_week14_makeupshades.jpeg", device = "jpeg")
