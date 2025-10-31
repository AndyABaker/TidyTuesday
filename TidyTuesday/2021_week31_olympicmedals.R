library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggExtra)
library(ggtext)
library(ggforce)
library(ggnewscale)


tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics %>% 
  filter(year %in% c(1920:2016)) %>% 
  mutate(year = factor(year, ordered = TRUE)) %>% 
  group_by(id, name) %>% 
  mutate(age = mean(age, na.rm = TRUE),
         weight = mean(weight, na.rm = TRUE),
         height = mean(height, na.rm = TRUE),
         bmi = weight/(height/100)^2) %>% 
  drop_na() %>% 
  distinct()

olympics_sex <- tuesdata$olympics %>% 
  filter(year %in% c(1920, 2016)) %>% 
  group_by(sex, year) %>% 
  summarise(age_mean = mean(age, na.rm = TRUE),
         weight_mean = mean(weight, na.rm = TRUE),
         height_mean = mean(height, na.rm = TRUE),
         bmi_mean = weight_mean/(height_mean/100)^2,
         n = n()) %>% 
  drop_na() %>% 
  distinct() %>% 
  arrange(sex, year)

olympics %>% 
  ggplot() + 
  geom_point(aes(weight, height, colour = sex, alpha = sex), 
             position = position_jitter(width = 0.5, height = 0.5, 
                                        seed = 10)) + 
  scale_colour_manual(values = c("#DA4167", "#083D77")) + 
  scale_alpha_discrete(range = c(0.35, 0.1)) + 
  new_scale_color() + 
  geom_line(data = olympics_sex,
            aes(weight_mean, height_mean, 
                group = sex, colour = sex),
            arrow = arrow(length = unit(.1, "inches"), 
                          type = "closed",
                          angle = 20)) + 
  scale_size(range = c(0.1, 1)) + 
  scale_colour_manual(values = c("#F3BAC6", "#B4D6F8")) + 
  scale_x_continuous(breaks = seq(25, 175, 25)) + 
  scale_y_continuous(breaks = seq(150, 225, 25)) + 
  coord_equal() + 
  labs(title = "Height and weight of
  **<span style='color:#DA4167;'>female</span>** and
  **<span style='color:#083D77;'>male</span>** olympic athletes, 1920-2016.",
       x = "Weight (kg)", 
       y = "Height (cm)", 
       caption = "Each athlete is represented by a single mean point.<br>
       Two lines represent the change in means, from 1920 and 2016.<br>
       **Source:** Kaggle | **Visualisation:** @Andy_A_Baker") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#FCF4F6", colour = NA),
        plot.background = element_rect(fill = "#FCF4F6", colour = NA),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(family = "Rubik", face = "bold", 
                                            lineheight = 0.35, size = 94,
                                            halign = 0.5, 
                                            margin = margin(0, 0, 10, 0)),
        text = element_text(family = "Rubik", colour = "#041E39", size = 45),
        axis.text = element_text(family = "Rubik", colour = "#041E39", 
                                 size = 45),
        axis.text.x = element_text(margin = margin(0, 0, 5, 0)),
        plot.caption = element_markdown(family = "Rubik", colour = "#041E39",
                                        lineheight = 0.35, size = 45,
                                        margin = margin(-10, 0, 0, 0)),
        plot.margin = margin(10, 10, 10, 10))

ggsave(dpi = 300, width = 15.28, height = 10, units = "in", type = "cairo",
       filename = "2021_week31_olympicmedals.jpeg", 
       device = "jpeg")
