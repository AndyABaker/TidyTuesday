library(tidyverse)
library(janitor)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggsankey)
library(scales)
library(ggtext)


soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

(top <- soybean_use %>% 
    filter(!is.na(code),
           code != "OWID_WRL",
           year == 2013) %>% 
    group_by(entity) %>% 
    mutate(total_use = sum(human_food, animal_feed, processed, na.rm = TRUE)) %>% 
    arrange(desc(total_use)) %>% 
    ungroup() %>% 
    select(entity, total_use) %>% 
    distinct() %>% 
    arrange(total_use) %>% 
    mutate(row = row_number(),
           label_pos = cumsum(total_use)-(total_use/2)))

d_soy <- soybean_use %>% 
  filter(!is.na(code),
         code != "OWID_WRL") %>% 
  group_by(entity, year) %>% 
  mutate(total_use = sum(human_food, animal_feed, processed, na.rm = TRUE),
         fill_entity = case_when(entity == "China" ~ "#B43718", 
                                 entity == "India" ~ "#D7431D", 
                                 entity == "Japan" ~ "#E45C3A", 
                                 entity == "Taiwan" ~ "#E9795D", 
                                 entity == "Indonesia" ~ "#EE9781", 
                                 entity == "Thailand" ~ "#EE9781",
                                 
                                 entity == "Russia" ~ "#33014A",
                                 
                                 entity == "United States" ~ "#023579",
                                 entity == "Canada" ~ "#0247A1",
                                 
                                 entity == "Brazil" ~ "#11403B", 
                                 entity == "Argentina" ~ "#1A615A", 
                                 entity == "Mexico" ~ "#228177", 
                                 entity == "Paraguay" ~ "#2BA195", 
                                 entity == "Bolivia" ~ "#33C1B3", 
                                 
                                 entity == "Germany" ~ "#4997FD", 
                                 entity == "Spain" ~ "#72AEFE", 
                                 entity == "Netherlands" ~ "#9AC5FE", 
                                 entity == "Italy" ~ "#C2DCFE", 
                                 
                                 entity == "Egypt" ~ "#F7C8A1", 
                                 entity == "Turkey" ~ "#FADEC7", 
                                 
                                 !(entity %in% c("China", "India", "Japan", "Taiwan", "Indonesia", "Thailand", 
                                                 "Russia", "United States", "Canada", 
                                                 "Brazil", "Argentina", "Mexico", "Paraguay", "Bolivia",
                                                 "Germany", "Spain", "Netherlands", "Italy",
                                                 "Egypt", "Turkey")) ~ "#e5e5e5")) %>% 
  ungroup() %>% 
  left_join(top, by = "entity")

d_soy %>% 
  filter(year %in% c(1961, 2013)) %>% 
  group_by(year == 2013) %>% 
  summarise(total_use = sum(total_use.x))

rm(top, soybean_use)

p <- d_soy %>% 
  ggplot(aes(year, node = entity, fill = fill_entity, colour = fill_entity, value = total_use.x)) + 
  geom_sankey_bump(space = 0, type = "alluvial", color = "transparent", smooth = 6, alpha = 0.8) + 
  annotation_custom(grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")), xmin = 2013.4) + 
  geom_text(data = subset(d_soy, year == 2013 & fill_entity != "#e5e5e5"), aes(y = label_pos, label = entity), nudge_x = 0.5, hjust = 0, size = 8) + 
  annotate(geom = "text", label = "Between 1961 and 2013,\nglobal soybean use increased\nfrom 22 million tonnes to",
                    x = 1963, y = 225000000, hjust = 0, lineheight = 0.33, size = 35, colour = "black") + 
  annotate(geom = "text", label = "bold('255 million')",
           x = 1963, y = 186000000, hjust = 0, lineheight = 0.33, size = 60, colour = "black", parse = TRUE) + 
  annotate(geom = "text", label = "bold('tonnes.')",
           x = 1963, y = 164000000, hjust = 0, lineheight = 0.33, size = 70, colour = "black", parse = TRUE) +
  scale_fill_identity() + 
  scale_colour_identity() + 
  scale_y_continuous(breaks = seq(0, 250000000, 50000000), labels = seq(0, 250, 50), expand = c(0, 0)) + 
  scale_x_continuous(breaks = seq(1960, 2010, 5), labels = seq(1960, 2010, 5), limits = c(1961, 2018), expand = c(0, 0)) + 
  labs(y = "Soybean use after trade* (million tonnes)",
       x = NULL,
       caption = "*Soybean production minus exports plus imports.\nSource: Our World in Data | Visualisation: @Andy_A_Baker") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey95"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(colour = "black", family = "Rubik", size = 40),
        axis.text = element_text(colour = "black", family = "Rubik"),
        axis.line.x = element_blank(),
        plot.caption = element_text(size = 30, lineheight = 0.3))


ggsave(p, dpi = 300, width = 10, height = 10, units = "in", filename = "2021_week15_globaldeforestation.jpeg", device = "jpeg")
