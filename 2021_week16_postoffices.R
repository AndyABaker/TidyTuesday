library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(viridis)
library(gganimate)
library(gifski)
library(rcartocolor)


post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>% 
  filter(duration >= 0 & duration < 212)

post_offices %>% 
  pivot_longer(cols = established:discontinued, names_to = "year_d", values_to = "year") %>% 
  ggplot(aes(year, colour = year_d, fill = year_d)) + 
  geom_histogram(binwidth = 1)

p <- post_offices %>% 
  filter(longitude < 0 & longitude > -130) %>% 
  select(longitude, latitude, gnis_elev_in_m, established, discontinued, duration) %>% 
  ggplot(aes(longitude, latitude, colour = gnis_elev_in_m)) + 
  geom_point(size = 0.5, alpha = 0.8) + 
  scale_color_carto_c(name = "Elevation (m)", type = "quantitative", palette = "ag_GrnYl") + 
  theme_void() + 
  coord_fixed() + 
  labs(title = "US Post Offices, 1764-2002",
       subtitle = "Year: {as.integer(frame_time)}",
       caption = "Source: Cameron Blevins and Richard W. Helbock | Visualisation: @Andy_A_Baker") + 
  theme(plot.title = element_text(colour = "black", family = "Rubik", size = 60),
        plot.subtitle = element_text(colour = "black", family = "Rubik", size = 40),
        plot.caption = element_text(colour = "black", family = "Rubik", size = 30)) + 
  transition_events(start = established,
                    end = established + duration,
                    enter_length = 0,
                    exit_length = 0)


animate(p, duration = 30, fps = 30, width = 2400, height = 1200, renderer = gifski_renderer(loop = TRUE), end_pause = 15)
anim_save("2021_week16_postoffices.gif")
