library(tidyverse)
library(janitor)
library(ggrepel)


hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  clean_names()

hs_students <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hs_students.csv') %>% 
  clean_names()


hs_s <- hs_students %>% 
  mutate(total = if_else(total > 10000, str_sub(total, 1, 4) %>% as.double(), total)) %>% 
  rename(year = total) %>% 
  select(!contains("standard")) %>% 
  select(!contains("total")) %>% 
  mutate(across(white1:last_col(), as.double)) %>% 
  mutate(rel_asian_pacific_islander_asian = ((asian_pacific_islander_asian + asian_pacific_islander_pacific_islander)/2) - white1, 
         rel_american_indian_alaska_native = american_indian_alaska_native - white1, 
         rel_black1 = black1 - white1, 
         rel_hispanic = hispanic - white1, 
         rel_two_or_more_race = two_or_more_race - white1) %>% 
  pivot_longer(cols = rel_asian_pacific_islander_asian:last_col(), names_to = "group", values_to = "rel_percentage") %>% 
  mutate(group = factor(group, levels = c("rel_asian_pacific_islander_asian", 
                                          "rel_american_indian_alaska_native", 
                                          "rel_black1", 
                                          "rel_hispanic", 
                                          "rel_two_or_more_race"), ordered = T)) %>% 
  filter(!is.na(rel_percentage))

levels(hs_s$group) <- c("Asian/Pacific Islander",
                        "American Indian/\nAlaska Native",
                        "Black",
                        "Hispanic",
                        "Two or more race")


hs_s <- hs_s %>% mutate(label = ifelse(year == 2016, paste(round_half_up(rel_percentage),"%", ", ",group, sep = ""), NA))

(p <- hs_s %>% 
    ggplot(aes(x = year, y = rel_percentage, color = group)) +
    geom_line(size = 1.3) + 
    # geom_text_repel(aes(label = label), nudge_x = 1.5, na.rm = T, segment.color = "transparent") + 
    geom_text(data = hs_s %>% filter(year == last(year)), aes(label = label, 
                                                                 x = year + 0.5, 
                                                                 y = rel_percentage, 
                                                                 color = group), hjust = 0) + 
    scale_x_continuous(breaks = seq(1975,2016,5), limits = c(1975, 2023)) + 
    geom_hline(yintercept = 0) + 
    scale_colour_manual(values = c("#af5b74", "#7a9bb2", "#153131", "#af8000", "#1a5d97")) + 
    theme_minimal() + 
    labs(title = "Bachelor's degree attainment of students age 25 and over, relative to white students 1975-2016",
         y = "Bachelor's degree attainment, relative to white students (%)\n",
         caption = "Source: Data.World | Visualisation: @Andy_A_Baker") + 
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "#F7F8F2", colour = "#F7F8F2"),
      plot.background = element_rect(fill = "#F7F8F2"),
      text = element_text(family = "ITC Officina Sans"),
      axis.text.y = element_text(face = "plain", colour = "#020F12"),
      axis.text.x = element_text(face = "bold", colour = "#020F12"),
      axis.title.y = element_text(face = "bold", colour = "#020F12"),
      axis.title.x = element_blank()))

ggsave(p, dpi = 300, width = 12, height = 10, units = "in", filename = "2021_week06_hbcuenrollment.jpeg", device = "jpeg")
