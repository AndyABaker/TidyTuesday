library(tidyverse)
library(janitor)
library(forcats)
library(ggtext)

income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv') %>% 
  filter(race %in% c("White Alone", "Black Alone", "Asian Alone", "Hispanic (Any Race)")) %>% 
  mutate(income_bracket = factor(income_bracket, levels = c("Under $15,000", 
                                                            "$15,000 to $24,999", 
                                                            "$25,000 to $34,999", 
                                                            "$35,000 to $49,999", 
                                                            "$50,000 to $74,999", 
                                                            "$75,000 to $99,999",
                                                            "$100,000 to $149,999",
                                                            "$150,000 to $199,999",
                                                            "$200,000 and over"), ordered = T),
         race = factor(race, levels = c("Black Alone", "Hispanic (Any Race)", "White Alone", "Asian Alone"), ordered = T))


means <- income_distribution %>% 
  group_by(income_bracket, race) %>% 
  mutate(min_year = min(year),
         max_year = max(year)) %>% 
  filter(year == min_year | year == max_year) %>% 
  group_by(income_bracket, race, year) %>% 
  mutate(income_dist = income_distribution)

income_distribution %>% 
  arrange(race) %>% 
  mutate(income_dist = income_distribution) %>% 
  ggplot() + 
  geom_point(aes(income_dist, income_bracket, fill = race, colour = race, size = year, group = race), shape = 21, alpha = 0.4, position = position_dodge(width = 0.75)) + 
  geom_point(data = means, aes(income_dist, income_bracket, size = year, group = race), shape = 21, colour = "black", position = position_dodge(width = 0.75)) + 
  scale_x_continuous(breaks = seq(0, 30, 5)) + 
  labs(x = "\nIncome distribution (%)",
       y = "Income Bracket",
       title = "<span style='font-size:16pt'>Income distribution across
       **<span style='color:#E07A5F;'>Asian</span>**, 
       **<span style='color:#81B29A;'>White</span>**, 
**<span style='color:#F2CC8F;'>Hispanic</span>**, and 
**<span style='color:#3D405B;'>Black</span>** households in the US from 1967* to 2019
  </span>",
       caption = "Source: The Urban Institute and the US Census | Visualisation: @Andy_A_Baker\n*1972 for Hispanic and 1987 for Asian.") + 
  annotate(geom = "curve", x = 21, y = 9.2, xend = 19.4, yend = 9.29, curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 21.2, y = 9, label = "In 2019, 19% of Asian households\nhad a total income of $200,000\nor over, up from only 5% in 1987.", hjust = "left") + 
  annotate(geom = "curve", x = 1.8, y = 5.4, xend = 2.6, yend = 6.65, curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1.8, y = 5, label = "In 1967, less than 3% of Black households\nhad a total income of $100,000-$149,000.\nThis had increased to 11% by 2019.", hjust = "left") + 
  scale_colour_manual(values = c("#3D405B", "#F2CC8F", "#81B29A", "#E07A5F")) + 
  scale_fill_manual(values = c("#3D405B", "#F2CC8F", "#81B29A", "#E07A5F")) + 
  scale_size_continuous(range = c(0.8,7)) + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid = element_line(colour = "#f3f5f7"),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "ITC Officina Sans"),
        plot.title = element_textbox_simple(face = "bold", padding = margin(10, 0, 10, 0)), 
        axis.text = element_text(face = "plain", colour = "black", size = 12),
        axis.title = element_text(face = "bold", colour = "black", size = 12),
        plot.caption = element_text(size = 10))

ggsave(dpi = 300, width = 14, height = 10, units = "in", filename = "2021_week07_wealthandincome.jpeg", device = "jpeg")
