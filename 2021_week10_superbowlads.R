library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggstream)
# remotes::install_github("davidsjoberg/ggstream")

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

youtube %>% 
  select(year, brand, funny:use_sex, like_count, view_count) %>% 
  pivot_longer(cols = funny:use_sex, names_to = "type") %>% 
  filter(value == T) %>% 
  mutate(type = factor(case_when(type == "animals" ~ "Contains animals",
                                 type == "celebrity" ~ "Contains celebrity",
                                 type == "danger" ~ "Contains danger",
                                 type == "funny" ~ "Contains humor",
                                 type == "patriotic" ~ "Patriotic",
                                 type == "show_product_quickly" ~ "Shows product quickly",
                                 type == "use_sex" ~ "Uses sexuality")),
         brand = ifelse(brand == "Hynudai", "Hyundai", brand),
         brand = factor(brand, levels = c("Coca-Cola", "Pepsi", "Bud Light", "Budweiser", "Doritos", "NFL", "E-Trade", "Toyota", "Hyundai", "Kia"), ordered = T)) %>% 
  group_by(year, brand, type) %>% 
  summarise(rel_likes = sum(like_count, na.rm = T)/sum(view_count, na.rm = T)) %>% 
  ggplot(aes(year, rel_likes, fill = brand)) + 
  geom_stream(extra_span = 0.1, 
              true_range = "both") + 
  coord_flip() + 
  labs(title = "Most liked Superbowl commercial types, by brand, from 2000-2020",
       fill = "",
       caption = "Uses like:view ratios. Smoothing effect makes ends inaccurate. \nSource: FiveThirtyEight | Visualisation: @Andy_A_Baker") + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(limits = c(2000, 2020), expand = c(0, 0), breaks = seq(2000, 2020, 1)) + 
  scale_fill_manual(values = c("#9d0208", "#dc2f02", "#f48c06", "#ffba08", "#d9ed92", "#f1faee", "#2a9d8f", "#0077b6", "#023e8a", "#03045e")) + 
  facet_wrap(vars(type), nrow = 1) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = NA),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = NA),
        text = element_text(size = 50),
        legend.spacing.x = unit(0.25, "cm"),
        plot.caption = element_text(size = 30, lineheight = 0.3),
        plot.title = element_text(face = "bold", size = 80),
        plot.margin = margin(10, 10, 5, 10))

ggsave(dpi = 300, width = 15, height = 10, units = "in", filename = "2021_week10_superbowlads.jpeg", device = "jpeg")
