library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(Cairo)
library(viridis)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)
animal_rescues <- tuesdata$animal_rescues

wards <- sf::st_read("https://opendata.arcgis.com/datasets/60ea78fd4f9d47099adfa63c2ccbc8bf_0.geojson")
regions <- sf::st_read("https://opendata.arcgis.com/datasets/cfa25518ddd7408a8da5c27eb42dd428_0.geojson")

london <- regions %>% 
  filter(RGN20NM == "London")

animal_rescues_wards <- animal_rescues %>%
  select(ward_code, ward) %>% 
  mutate(ward = str_to_title(tolower(ward))) %>% 
  count(ward_code, ward)

animal_wards_london <- wards %>% 
  filter(sf::st_within(wards, london, sparse = FALSE)) %>% 
  left_join(animal_rescues_wards, by = c("WD20CD" = "ward_code"))

(hist <- animal_wards_london %>% 
    drop_na() %>% 
    ggplot(aes(n, fill = ..x..)) + 
    geom_histogram(breaks = seq(0, 45, 2.5)) + 
    scale_y_continuous(expand = c(0.025, 0)) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_fill_viridis_c(option = "E") + 
    theme_void() + 
    labs(x = "Count of rescues") + 
    theme(legend.position = "none", 
          axis.text.x = element_text(family = "Rubik", size = 35)))

(map <- animal_wards_london %>% 
    ggplot() + 
    geom_sf(aes(fill = n), lwd = NA) + 
    scale_fill_viridis_c(option = "E") + 
    scale_x_continuous(limits = c(-0.483, 0.28)) + 
    theme_void() + 
    labs(title = "Animal rescues by the London Fire Brigade, 2009-2019",
         caption = "Source: London.gov | Visualisation: @Andy_A_Baker")) + 
  theme(legend.position = "none",
        plot.caption = element_text(family = "Rubik", size = 35),
        plot.title = element_text(face = "bold", size = 70, hjust = 0.5),
        panel.background = element_rect(fill = "#F7F6F3", colour = NA),
        plot.background = element_rect(fill = "#F7F6F3", colour = NA),
        panel.border = element_blank(),
        plot.margin = margin(10, 10, 10, 10)) +  
  inset_element(hist, left = 0, bottom = -0.02, right = 0.4, top = 0.35)

ggsave(dpi = 300, width = 12, height = 10.84, units = "in", type = "cairo",
       filename = "2021_week27_animalrescues.jpeg", device = "jpeg")
