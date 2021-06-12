library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(janitor)
library(sf)
library(rnaturalearth)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 24)
stocked <- tuesdata$stocked %>% clean_names()

lakes <- ne_download(scale = 50, 
                     type = "lakes", 
                     category = "physical", 
                     returnclass = "sf")

lake_colours <- c("#00383D",
                  "#FFD1C2",
                  "#D86D46",
                  "#66B7AF",
                  "#C1E1EB",
                  "#9D0615")

lake_names <- c("Erie",
                "Huron",
                "Michigan",
                "Ontario",
                "Saint Clair",
                "Superior")

# Make map legend ---------------------------------------------------------

great_lakes <- lakes %>%
  mutate(lake = stringr::str_remove(name, "Lake ")) %>%
  filter(lake %in% c("Erie",
                     "Huron",
                     "Michigan",
                     "Ontario",
                     "Saint Clair",
                     "Superior")) %>% 
  mutate(lake = factor(lake, levels = lake_names,
                       ordered = TRUE))

legend <- great_lakes %>% 
  ggplot() + 
  geom_sf(aes(fill = lake), lwd = NA) + 
  scale_fill_manual(values = lake_colours) + 
  theme_void() + 
  theme(legend.position = "none")


# Make final plot ---------------------------------------------------------

stocked %>% 
  filter(species == "RBT",
         length != 0,
         weight != 0) %>% 
  mutate(lake = factor(case_when(lake == "ER" ~ "Erie",
                                 lake == "HU" ~ "Huron",
                                 lake == "MI" ~ "Michigan",
                                 lake == "ON" ~ "Ontario",
                                 lake == "SC" ~ "Saint Clair",
                                 lake == "SU" ~ "Superior"),
                       levels = lake_names,
                       ordered = TRUE)) %>% 
  ggplot(aes(length, weight, size = no_stocked, colour = lake)) + 
  geom_point(alpha = 0.5) + 
  scale_x_log10(limits = c(20, 600)) + 
  scale_y_log10(breaks = c(1, 100, 10000), labels = c("1", "100", "10,000")) + 
  scale_colour_manual(values = lake_colours) + 
  scale_size(breaks = c(100, 1000, 10000, 100000), 
             labels = c("100 fish", 
                        "1,000 fish", 
                        "10,000 fish", 
                        "100,000 fish"), 
             range = c(0.5, 10),
             guide = guide_legend(reverse = TRUE)) + 
  annotation_custom(ggplotGrob(legend), 
                    xmin = 0.6, xmax = 3.32, 
                    ymin = -1.7, ymax = 0.8) + 
  labs(
    title = "Rainbow Trout stocked in the Great Lakes, 1987-2018",
    y = "Average stock weight (kg)",
    x = "Average fish length (mm)",
    size = NULL,
    caption = "Source: Great Lakes Database | Visualisation: @Andy_A_Baker"
    ) + 
  guides(colour = FALSE) + 
  theme(text = element_text(colour = "black", family = "Rubik", size = 50),
        legend.position = c(0.889, 0.16),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(margin = margin(l = -20)),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(margin = margin(b = 7, t = 7)),
        plot.title = element_text(face = "bold", size = 70),
        plot.margin = margin(10, 10, 10, 10))

ggsave(dpi = 300, width = 10, height = 10, units = "in", 
       filename = "2021_week24_greatlakesfish.jpeg", device = "jpeg")

