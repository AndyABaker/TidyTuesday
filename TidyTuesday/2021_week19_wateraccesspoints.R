library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(rgdal)
library(sf)
library(biscale)
library(cowplot)


# Load data ---------------------------------------------------------------

# https://africaopendata.org/dataset/shape-file-of-nigeria
nigeria <- readOGR("Shapefile19/new_lga_nigeria_2003.shp") 
nigeria_sf <- st_as_sf(nigeria)

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)
water <- tuesdata$water %>% 
  filter(country_name == "Nigeria",
         water_source %in% c("Borehole", 
                             "Protected Shallow Well", 
                             "Protected Spring"),
         lon_deg > 2.69,
         lon_deg < 14.68,
         lat_deg > 4.26,
         lat_deg < 13.72) %>% 
  mutate(water_source_f = case_when(
    water_source == "Borehole" ~ "Borehole",
    water_source %in% c("Protected Shallow Well", "Protected Spring") ~ 
      "Protected shallow well or spring"))


# Data prep ---------------------------------------------------------------

water_sf <- st_as_sf(water, coords = c("lon_deg", "lat_deg"), crs = 4326)

water_lga <- st_within(water_sf, nigeria_sf, sparse = FALSE)

water_lga_b <- st_within(subset(water_sf, water_source_f == "Borehole"), 
                         nigeria_sf, sparse = FALSE)

water_lga_p <- st_within(
  subset(water_sf, water_source_f == "Protected shallow well or spring"), 
  nigeria_sf, sparse = FALSE)

nigeria_sf <- nigeria_sf %>%
  mutate(count = apply(water_lga, 2, sum),
         count_b = apply(water_lga_b, 2, sum),
         count_p = apply(water_lga_p, 2, sum),
         prop_b = if_else(count == 0, 0, count_b/count),
         prop_p = if_else(count == 0, 0, count_p/count))


# Bivariate map -----------------------------------------------------------

nigeria_bi <- bi_class(nigeria_sf, x = count_p, y = count_b, 
                       style = "quantile", dim = 3)

p <- ggplot() +
  geom_sf(data = nigeria_bi, mapping = aes(fill = bi_class), 
          color = "grey80", size = 0.01, show.legend = FALSE) +
  geom_point(data = water, aes(lon_deg, lat_deg), 
             size = 0.01, alpha = 0.1) + 
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  bi_theme() + 
  labs(title = "Water access points across Local Government Areas in Nigeria",
       caption = "Source: WPDx | Visualisation: @Andy_A_Baker") + 
  theme(title = element_text(size = 100, family = "Rubik"),
        plot.caption = element_text(size = 55, colour = "grey40", family = "Rubik"),
        axis.title = element_blank())

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "More Boreholes",
                    ylab = "More wells or springs",
                    size = 55)

p_final <- ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, 0.1, 0.2, 0.2)

ggsave(p_final, dpi = 300, width = 20, height = 20, units = "in", 
       filename = "2021_week19_wateraccesspoints.jpeg", device = "jpeg")
