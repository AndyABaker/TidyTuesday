library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(lubridate)
library(scales)
library(patchwork)

# read in data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv') %>% 
  mutate(gamename = factor(gamename),
         avg_peak_p = (avg/peak)*100) %>% 
  unite(yearmonth, month:year) %>%
  mutate(date = my(yearmonth),
         year = year(date)) %>%
  select(-avg_peak_perc) %>% 
  group_by(year, gamename) %>% 
  mutate()


# get most popular games
top_games <- games %>% 
  group_by(gamename) %>% 
  summarise(mean_avg = mean(avg),
            total_avg = sum(avg),
            max_peak = max(peak)) %>% 
  arrange(desc(total_avg)) %>% 
  slice_head(n = 3)

games_f <- games %>% 
  mutate(in_top_games = ifelse(gamename %in% top_games$gamename, 1, 0)) %>% 
  group_by(gamename) %>% 
  mutate(release_avg_point = ifelse(date == min(date) & date != "2012-07-01", avg, NA),
         release_avg = ifelse(date == min(date) & date != "2012-07-01", avg, NA),
         release_rel_date = interval(min(date), date) %/% months(1)) %>% 
  fill(release_avg, .direction = "up") %>% 
  group_by(date) %>% 
  mutate(norm_avg = avg-release_avg) %>% 
  ungroup() %>% 
  filter(!is.na(release_avg)) %>% 
  group_by(date) %>% 
  mutate(avg_all = median(avg))

(p1 <- games_f %>% 
    ggplot(aes(date, avg)) + 
    geom_line(data = subset(games_f, in_top_games == 0), aes(group = gamename), alpha = 0.1, colour = "black") + 
    geom_point(data = subset(games_f, in_top_games == 0), aes(date, release_avg_point), alpha = 0.3, colour = "black") + 
    geom_line(data = games_f, aes(date, avg_all), colour = "black", size = 1.5) + 
    geom_point(data = games_f, aes(date, avg_all), colour = "black", size = 1) + 
    geom_point(data = subset(games_f, in_top_games == 1), aes(date, release_avg_point, colour = gamename), size = 2) + 
    geom_line(data = subset(games_f, in_top_games == 1), aes(colour = gamename), size = 1) + 
    scale_y_log10(labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1,000", "10,000", "100,000", "1,000,000"),
                  breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)) + 
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y", expand = c(0, 1)) + 
    labs(y = "Average number of players at one time",
         x = "") + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = NA),
          text = element_text(colour = "black", family = "rubik", size = 45),
          axis.text = element_text(colour = "black", family = "rubik", size = 45),
          plot.margin = margin(0, 15, 0, 0)
    ))


(p2 <- games_f %>% 
    ggplot(aes(release_rel_date, norm_avg)) + 
    geom_line(data = subset(games_f, in_top_games == 1), aes(colour = gamename), size = 1) + 
    geom_line(data = subset(games_f, in_top_games == 0), aes(group = gamename), alpha = 0.1, colour = "black") + 
    scale_y_continuous(labels = comma_format(digits = 0), breaks = seq(-300000, 1500000, 100000)) + 
    scale_x_continuous(labels = comma_format(digits = 0), breaks = seq(0, 100, 10), expand = c(0, 1)) + 
    annotate(geom = "curve", x = 28, y = 1400000, xend = 11, yend = 1567335.63, curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = 15, y = c(1360000, 1310000), size = 14, label = c("PUBG had an average of 1,584,887 players in January 2018.", "This was over 9000 times higher than the average in the release month."), hjust = "left") + 
    coord_cartesian(xlim = c(NA, 50)) + 
    labs(y = "Average number of players at one time, relative to the release month",
         x = "Months since release") + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = NA),
          text = element_text(colour = "black", family = "rubik", size = 45),
          axis.text = element_text(colour = "black", family = "rubik", size = 45),
          plot.margin = margin(0, 0, 0, 15)
    ))

(p <- (p1 | p2) + 
    plot_annotation(title = "The unprecedented rise of PlayerUnknown's Battlegrounds (PUBG)",
                    caption = "Only including games released after August 2012. The median average number of players is given as a thick black line. Dots indicate a game's release.\nSource: Steam | Visualisation: @Andy_A_Baker",
                    theme = theme(plot.margin = margin(10, 10, 10, 10))) & 
    theme(plot.title = element_text(family = "rubik", face = "bold", size = 138),
          plot.caption = element_text(size = 35, lineheight = 0.3)))


ggsave(p, dpi = 300, width = 20, height = 12, units = "in", filename = "2021_week12_videogamessliced.jpeg", device = "jpeg")
