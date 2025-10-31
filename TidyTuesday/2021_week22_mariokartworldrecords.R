library(tidytuesdayR)
library(tidyverse)
library(ggflags)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(ggforce)


tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
records <- tuesdata$records
drivers <- tuesdata$drivers %>% 
    mutate(country = case_when(
        nation == "USA" ~ "us",
        nation == "Australia" ~ "au",
        nation == "Canada" ~ "ca",
        nation == "Netherlands" ~ "nl",
        nation == "UK" ~ "gb",
        nation == "Brazil" ~ "br",
        nation == "Germany" ~ "de",
        nation == "Austria" ~ "at",
        nation == "Croatia" ~ "hr",
        nation == "France" ~ "fr",
        nation == "Ireland" ~ "ie",
        nation == "Norway" ~ "no",
        nation == "Slovenia" ~ "si"
    )) %>% 
    select(player, nation, country) %>% 
    distinct()

records_n <- left_join(records, drivers, by = "player")


records_n %>% 
    filter(track == "Wario Stadium",
           type == "Single Lap",
           !is.na(nation)) %>% 
    group_by(track) %>% 
    mutate(time_rel = time-max(time),
           time_rel_prop = (max(time)-time)/max(time)) %>% 
    ggplot(aes(date, time)) + 
    geom_line(alpha = 0.5) + 
    geom_flag(aes(country = country)) + 
    scale_country() + 
    scale_y_continuous(breaks = pretty) + 
    facet_zoom(ylim = c(85.75, 88)) + 
    theme_bw() + 
    labs(
        title = "Wario Stadium World Records, 1997-2021",
        y = "Track time (s)",
        x = NULL,
        caption = 
            "Source: Mario Kart World Records | Visualisation: @Andy_A_Baker") + 
    theme(legend.position = "none",
          panel.grid.major.y = element_line(colour = "grey95"),
          panel.grid.minor.x = element_blank(),
          text = element_text(colour = "black", family = "Rubik", 
                              size = 40),
          axis.text = element_text(colour = "black", family = "Rubik"),
          axis.text.x = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          plot.title = element_text(size = 80, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 30, lineheight = 0.3),
          plot.margin = margin(10, 10, 10, 10))


ggsave(dpi = 300, width = 10, height = 10, units = "in", 
       filename = "2021_week22_mariokartworldrecords.jpeg", device = "jpeg")


# REMEMBER TO include alt text!