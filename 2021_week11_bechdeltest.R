library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggbeeswarm)
library(DescTools)
library(ggtext)


movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv') %>% 
  mutate(clean_test_f = factor(case_when(clean_test == "ok" ~ "Pass",
                                         clean_test == "dubious" ~ "Dubious",
                                         clean_test == "men" ~ "Women only talk\nabout men",
                                         clean_test == "notalk" ~ "Women don't talk\nto each other",
                                         clean_test == "nowomen" ~ "Fewer than two\nwomen"), 
                               levels = c("Pass", 
                                          "Dubious", 
                                          "Women only talk\nabout men", 
                                          "Women don't talk\nto each other", 
                                          "Fewer than two\nwomen"), ordered = T),
         intgross_2013 = as.numeric(intgross_2013),
         budget_2013 = as.numeric(budget_2013),
         #intprofit_2013 = intgross_2013-budget_2013,
         intprofitratio_2013 = intgross_2013/budget_2013,
         #runtime_n = as.numeric(substr(runtime, 1, nchar(runtime)-4)),
         decade = factor(case_when(year >= 1970 & year < 2000 ~ "1970-90s",
                                   year >= 2000 & year < 2010 ~ "2000s",
                                   year >= 2010 & year < 2020 ~ "2010s")),
         action = ifelse(genre %like% "%Action%", 1, 0),
         adventure = ifelse(genre %like% "%Adventure%", 1, 0),
         comedy = ifelse(genre %like% "%Comedy%", 1, 0),
         crime = ifelse(genre %like% "%Crime%", 1, 0),
         drama = ifelse(genre %like% "%Drama%", 1, 0),
         horror = ifelse(genre %like% "%Horror%", 1, 0),
         scifi = ifelse(genre %like% "%Sci-Fi%", 1, 0),
         fantasy = ifelse(genre %like% "%Fantasy%", 1, 0),
         romance = ifelse(genre %like% "%Romance%", 1, 0),
         thriller = ifelse(genre %like% "%Thriller%", 1, 0),
         mystery = ifelse(genre %like% "%Mystery%", 1, 0),
         western = ifelse(genre %like% "%Western%", 1, 0),
         biography = ifelse(genre %like% "%Biography%", 1, 0),
         war = ifelse(genre %like% "%War%", 1, 0),
         history = ifelse(genre %like% "%History%", 1, 0),
         family = ifelse(genre %like% "%Family%", 1, 0), 
         sport = ifelse(genre %like% "%Sport%", 1, 0),
         music = ifelse(genre %like% "%Music%", 1, 0),
         musical = ifelse(genre %like% "%Musical%", 1, 0),
         documentary = ifelse(genre %like% "%Documentary%", 1, 0),
         animation = ifelse(genre %like% "%Animation%", 1, 0)) %>% 
  pivot_longer(cols = action:animation, names_to = "genre_f", values_to = "genre_l") %>% 
  filter(genre_l == 1) %>% 
  group_by(genre_f) %>% 
  mutate(genre_n = n()) %>% 
  filter(genre_n >= 300) %>% 
  ungroup() %>% 
  mutate(genre_f = case_when(genre_f == "action" ~ "Action",
                             genre_f == "adventure" ~ "Adventure",
                             genre_f == "comedy" ~ "Comedy",
                             genre_f == "drama" ~ "Drama",
                             genre_f == "thriller" ~ "Thriller"))


(p <- movies %>% 
  ggplot(aes(decade, intprofitratio_2013, fill = binary)) + 
  geom_hline(yintercept = 1, colour = "#C5DBE7") + 
  geom_quasirandom(aes(colour = binary), alpha = 0.4, dodge.width = 0.6) + 
  geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.6, width = 0.2, outlier.colour = NA) + 
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.6), shape = 4, size = 2) + 
  scale_colour_manual(values = c("#c1121f", "#0077B8")) + 
  scale_fill_manual(values = c("#c1121f", "#0077B8")) + 
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01, 0.1, 1, 10, 100)) + 
  facet_wrap(vars(genre_f), nrow = 1) + 
  labs(title = "<span style='font-size:80pt'>Movie profits by year, genre, and whether they 
       **<span style='color:#0077B8;'>passed</span>** or 
       **<span style='color:#c1121f;'>failed</span>** the Bechdel test
  </span>",
       y = "Dollars earned for every dollar spent",
       caption = "\nBased on the budget and international gross in release year, normalized to 2013 US dollars.\nSource: FiveThirtyEight | Visualisation: @Andy_A_Baker") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F7F7F7", colour = NA),
        strip.background = element_rect(fill = "#F7F7F7", colour = NA),
        plot.background = element_rect(fill = "#F7F7F7", colour = NA),
        panel.grid.major.y = element_line(colour = "#E2EDF3"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Rubik", size = 60),
        plot.title = element_textbox_simple(face = "bold", halign = 0.4, padding = margin(10, 0, 10, 0)),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        strip.text.x = element_text(face = "bold"),
        axis.text = element_text(face = "plain", colour = "black"),
        axis.title = element_text(face = "bold", colour = "black"),
        plot.caption = element_text(lineheight = 0.3, size = 30)))


ggsave(p, dpi = 300, width = 16, height = 10, units = "in", filename = "2021_week11_bechdeltest.jpeg", device = "jpeg")
