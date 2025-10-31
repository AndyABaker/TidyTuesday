library(tidyverse)
library(janitor)
library(showtext)

# https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge04/original-plate-51.jpg

freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv') %>% clean_names()

font_add_google("Public Sans")

freed_slaves <- freed_slaves %>% 
  mutate(slave = if_else(year == 1800, 89, slave))

freed_slaves %>% 
  pivot_longer(slave:free, names_to = "type", values_to = "prop") %>% 
  ggplot(aes(year, prop)) + 
  geom_area(aes(fill = type)) + 
  geom_label(aes(x = 1830, y = 96, label = "FREE — LIBRE"), family = "Public Sans", fontface = "bold", size = 28, label.size = NA, fill = NA) + 
  geom_label(aes(x = 1830, y = 55, label = "SLAVES"), family = "Public Sans", fontface = "bold", size = 40, label.size = NA, colour = "white", fill = NA) + 
  geom_label(aes(x = 1830, y = 50, label = "ESCLAVES"), family = "Public Sans", fontface = "bold", size = 40, label.size = NA, colour = "white", fill = NA) + 
  geom_label(data = subset(freed_slaves, year != 1870), aes(x = year, y = slave+1.75, label = paste(free, "%", sep = "")), family = "Public Sans", fontface = "bold", size = 18, label.size = NA, fill = NA) + 
  geom_label(data = subset(freed_slaves, year == 1870), aes(x = year, y = 90.75, label = paste(free, "%", sep = "")), family = "Public Sans", fontface = "bold", size = 18, label.size = NA, fill = NA) + 
  geom_segment(data = subset(freed_slaves, year %in% 1800:1860), aes(x = year, xend = year, y = slave+3, yend = 99.9), colour = "black", alpha = 0.3) + 
  scale_x_continuous(limits = c(1787.5,1872.5), breaks = seq(1790, 1870, 10), expand = c(0, 0), position = "top") + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = c("#2b8253", "black")) + 
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .",
       subtitle = "\nDONE BY ATLANTA UNIVERSITY .\n\n",
       caption = "Source: Du Bois Data Challenge | Visualisation: @Andy_A_Baker") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Public Sans", face = "bold", colour = "black", size = 65),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = "#daccc1", colour = NA),
        plot.background = element_rect(fill = "#daccc1", colour = NA),
        plot.title = element_text(family = "Public Sans", face = "bold", hjust = 0.5, size = 55),
        plot.subtitle = element_text(family = "Public Sans", face = "bold", hjust = 0.5, size = 35),
        plot.caption = element_text(face = "plain", family = "Public Sans", size = 30),
        plot.margin = margin(5, 10, 1, 10),
        plot.title.position = "plot") + 
  coord_fixed(ratio = 23.5/28)



ggsave(dpi = 300, width = 10.935, height = 14, units = "in", filename = "2021_week08_webduboischallenge.jpeg", device = "jpeg")
