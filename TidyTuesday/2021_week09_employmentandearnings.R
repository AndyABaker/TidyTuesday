library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(scales)
library(ggtext)


earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')


earn_f <- earn %>% 
  filter(race != "All Races",
         sex != "Both Sexes",
         age == "25 years and over",
         ethnic_origin == "All Origins") %>% 
  mutate(dodged_x = as.numeric(ifelse(race == "White", year, ifelse(race == "Black or African American", year-0.1, year+0.1))),
         quarter = factor(case_when(quarter == 1 ~ "\nQ1",
                                    quarter == 2 ~ "\nQ2",
                                    quarter == 3 ~ "\nQ3",
                                    quarter == 4 ~ "\nQ4"), ordered = T),
         sex = factor(sex),
         race = factor(race, levels = c("Asian", "White", "Black or African American"), ordered = T)) %>% 
  filter(quarter == "\nQ4") %>% 
  select(-age, -n_persons) %>% 
  pivot_wider(names_from = sex, values_from = median_weekly_earn)


earn_f %>% 
  ggplot() + 
  geom_segment(aes(x = dodged_x, xend = dodged_x, y = Men, yend = Women, colour = race)) + 
  geom_point(aes(dodged_x, y = Men, colour = race), size = 3, shape = 16) + 
  geom_point(aes(dodged_x, y = Women, colour = race), size = 3, shape = 18) + 
  scale_x_continuous(breaks = seq(2010, 2020, 1)) + 
  scale_y_continuous(breaks = seq(500, 1500, 100), labels = dollar) + 
  scale_colour_manual(values = c("#005E66", "#ACD8D3", "#A84724")) + 
  geom_richtext(aes(label = "<span style='font-size:55pt'>Men consistently had higher median weekly earnings than women from<br>
                            **<span style='color:#005E66;'>Asian</span>**, 
                            **<span style='color:#ACD8D3;'>White</span>**, and
                            **<span style='color:#A84724;'>Black or African American</span>** backgrounds.
                            <span style='font-size:55pt'><br>
                            <br>By 2020, Asian women earned $380 less than men, whilst<br>Black or African American women earned just $37 less.", 
                    x = 2013.5, y = 1400), label.color = NA) + 
  labs(y = "Median weekly earnings",
       caption = "Men plotted as circles, women plotted as diamonds. Data is for persons aged 25 years and over.\nSource: US Bureau of Labor Statistics | Visualisation: @Andy_A_Baker") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        text = element_text(family = "Rubik", size = 45),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "plain"),
        axis.text.y = element_text(face = "plain"),
        plot.caption = element_text(face = "plain", size = 30, lineheight = 0.3),
        panel.grid = element_line(colour = "#f3f5f7"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


ggsave(dpi = 300, width = 12, height = 10, units = "in", filename = "2021_week09_employmentandearnings.jpeg", device = "jpeg")
