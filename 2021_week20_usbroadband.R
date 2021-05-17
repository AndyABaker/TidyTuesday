library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(zipcodeR)
library(urbnmapr)

library(ggiraph)
library(cowplot)
library(colorspace)
library(tm)



# Data prep ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)
broadband <- tuesdata$broadband %>% 
  clean_names() %>% 
  mutate(bb_access = 100*as.numeric(broadband_availability_per_fcc),
         bb_usage = 100*as.numeric(broadband_usage),
         bb_diff = bb_usage-bb_access
         ) %>% 
  select(-broadband_availability_per_fcc, -broadband_usage)


# for county populations
counties_pop <- zipcodeR::zip_code_db %>%
  select(zipcode, county, state, population) %>% 
  group_by(state, county) %>% 
  summarise(population_county = sum(population))

# Map of US
counties_sf <- get_urbn_map("counties", sf = TRUE)

broadband_sf <- left_join(counties_sf, broadband, 
                          by = c("state_abbv" = "st", 
                                 "county_name"))

# Join to make final data. Note the use of <br> and not \n with ggiraph.
broadband_map <- left_join(broadband_sf, counties_pop, 
                           by = c("state_abbv" = "state", 
                                  "county_name" = "county")) %>% 
  mutate(state_name = removePunctuation(state_name),
         county_name = removePunctuation(county_name),
         id = paste0(state_name, ", ", county_name, 
                     "<br>Population: ", population_county,
                     "<br>Access to broadband speed: ", bb_access,"%",
                     "<br>Using broadband speed: ", bb_usage,"%"))



# Plotting ----------------------------------------------------------------

p_map_int <- broadband_map %>% 
  ggplot() + 
  geom_sf_interactive(aes(tooltip = id, data_id = id, fill = bb_diff)) + 
  scale_fill_stepsn(colors = diverge_hcl(8, rev = TRUE), 
                    breaks = c(-75, -50, -25, 0, 25, 50, 75), 
                    guide = guide_colorsteps(title.position = "top"), 
                    "Difference in percentage points") +
  labs(caption = "\n\n\n\n\n\n\n\n
       A download speed of 25 Mbps or above is considered 'Broadband speed'.
       \nSource: Microsoft GitHub | Visualisation: @Andy_A_Baker") + 
  theme_void() + 
  theme(legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, "line"),
        legend.key.width = unit(4, "line"),
        legend.title = element_text(family = "Rubik", size = 15, hjust = 0.5),
        legend.text = element_text(family = "Rubik", size = 15, hjust = 0.5),
        plot.caption = element_text(family = "Rubik", size = 15, hjust = 0.5, 
                                    lineheight = 0.45))

p_plot_int <- broadband_map %>% 
  ggplot(aes(bb_access, bb_usage, colour = bb_diff, size = population_county)) + 
  geom_jitter_interactive(aes(tooltip = id, data_id = id), 
    width = 0.01, height = 0.01, na.rm = TRUE, alpha = 0.8) + 
  scale_colour_stepsn(colors = diverge_hcl(8, rev = TRUE), 
                      breaks = c(-75, -50, -25, 0, 25, 50, 75)) +
  scale_size_continuous(range = c(1, 10)) + 
  scale_y_log10(breaks = c(1, 10, 100), labels = c(1, 10, 100)) + 
  scale_x_continuous(breaks = seq(0, 100, 25), labels = seq(0, 100, 25)) + 
  theme_minimal() + 
  labs(x = "\nHouseholds with access to broadband speed (%)",
       y = "Households using broadband speed (%)") + 
  theme(legend.position = "none",
        axis.text = element_text(family = "Rubik", size = 15),
        axis.title = element_text(family = "Rubik", size = 25),
        panel.grid.minor = element_blank())

girafe(
  ggobj = plot_grid(p_plot_int, p_map_int),
  options = list(
    opts_tooltip(use_fill = TRUE, offx = 20, offy = 20),
    opts_hover_inv(css = "opacity:0.5;"),
    opts_hover(css = "fill:yellow;stroke:yellow;"),
    opts_selection(type = "multiple", only_shiny = FALSE,
                   css = "fill:yellow;stroke:gray;")
  ),
  width_svg = 20,
  height_svg = 10
)

# For now, I've just recorded the viewer window instead of making a shiny app.