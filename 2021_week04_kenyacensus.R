library(tidyverse)
# devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(scatterpie)
library(sp)
library(sf)
library(rgdal)
library(ggforce)
library(ggnewscale)
library(patchwork)
library(scales)


# read in and tidy data

k_religion <- V4_T2.30 %>% 
  select(-"Total") %>% 
  filter(County != "KENYA") %>% 
  rename(county = County)

kenya <- readOGR("Shapefile/ke_county.shp") # https://data.humdata.org/dataset/47-counties-of-kenya
kenya_sf <- st_as_sf(kenya)
kenya_sf$county <- toupper(str_replace_all(kenya_sf$county, "[:punct:]", " "))
kenya_sf$pop.2009 <- as.numeric(kenya_sf$pop.2009)
CountyGPS <- CountyGPS %>% rename(county = County)
kenya_sf_religion <- left_join(k_religion, CountyGPS, by = "county")

kenya_sf_religion <- kenya_sf_religion %>% select("Longitude", "Latitude", "county", 
                                                  "Catholic", 
                                                  "Protestant", 
                                                  "Evangelical", 
                                                  "AfricanInstituted", 
                                                  "Orthodox", 
                                                  "OtherChristian", 
                                                  "Islam", 
                                                  "Hindu", 
                                                  "Traditionists", 
                                                  "OtherReligion", 
                                                  "NoReligion/Atheists", 
                                                  "Don't Know", 
                                                  "NotStated")
kenya_sf_religion$county <- factor(kenya_sf_religion$county)
kenya_sf_religion$Longitude[is.na(kenya_sf_religion$Longitude)] <- 36.817223
kenya_sf_religion$Latitude[is.na(kenya_sf_religion$Latitude)] <- -1.286389


# fix religion groupings and add population calculations

pop <- kenya_sf_religion %>% 
  group_by(county) %>% 
  select(-c("Longitude", "Latitude")) %>% 
  gather(key = religion, value = count, Catholic:NotStated) %>% 
  summarise(population = sum(count),
            radius = 0.1 + (population/47213282))

pop_other <- kenya_sf_religion %>% 
  group_by(county) %>% 
  select(-c("Longitude", "Latitude")) %>% 
  gather(key = religion, value = count, c("AfricanInstituted", 
                                          "OtherChristian",
                                          "Orthodox", 
                                          "Hindu", 
                                          "Traditionists", 
                                          "OtherReligion",
                                          "Don't Know", 
                                          "NotStated")) %>% 
  summarise(Other = sum(count))

kenya_sf_religion <- left_join(kenya_sf_religion, pop, by = "county")
kenya_sf_religion <- left_join(kenya_sf_religion, pop_other, by = "county")
kenya_sf_religion <- kenya_sf_religion %>% select(-c("AfricanInstituted", 
                                                     "OtherChristian",
                                                     "Orthodox", 
                                                     "Hindu", 
                                                     "Traditionists", 
                                                     "OtherReligion", 
                                                     "Don't Know", 
                                                     "NotStated"))

kenya_sf_religion <- kenya_sf_religion %>% rename(Atheist = 'NoReligion/Atheists')
kenya_sf_religion <- kenya_sf_religion %>% rename(Islamic = Islam)

# plotting

p1 <- ggplot() + 
  geom_sf(data = kenya_sf, aes(fill = pop.2009, lwd = NA)) + 
  scale_fill_gradient(low = "#D6F1FF", high = "#001B29", name = "Population", labels = comma) + 
  new_scale_fill() + 
  geom_scatterpie(aes(x = Longitude, y = Latitude, group = county, r = radius), alpha = 0.9, color = NA, data = kenya_sf_religion, cols = c("Protestant", 
                                                                                                                                           "Catholic", 
                                                                                                                                           "Evangelical",
                                                                                                                                           "Islamic", 
                                                                                                                                           "Atheist",
                                                                                                                                           "Other")) + 
  scale_fill_manual(values = c("#d62828", "#f77f00", "#fcbf49", "#6fae71", "#0077b8", "#FAF9EF")) + 
  labs(caption = "Source: rKenyaCensus | Visualisation: @Andy_A_Baker",
       fill = "Religion",
       colour = "Population") + 
  theme_void() + 
  theme(legend.position = c(0.25, 0.1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        panel.background = element_rect(fill = "#EBF8FF", colour = NA),
        plot.background = element_rect(fill = "#EBF8FF", colour = NA),
        panel.border = element_blank())

p2 <- kenya_sf_religion %>% 
  gather(key = religion, value = count, c("Protestant", 
                                          "Catholic", 
                                          "Evangelical",
                                          "Islamic", 
                                          "Atheist",
                                          "Other")) %>% 
  ggplot() + 
  geom_point(aes(population, count, colour = religion), alpha = 0.6, size = 3) + 
  scale_x_log10(labels = comma) + 
  scale_y_log10(labels = comma) + 
  #coord_fixed(ratio = 1, clip = "off") + 
  scale_colour_manual(values = c("#0077b8", "#f77f00", "#fcbf49", "#6fae71", "#FAF9EF", "#d62828")) + 
  labs(x = "Total county population",
       y = "Religious population") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#EBF8FF", colour = NA),
        plot.background = element_rect(fill = "#EBF8FF", colour = NA),
        text = element_text(family = "ITC Officina Sans"),
        axis.text.x = element_text(face = "plain", colour = "#000E14"),
        axis.text.y = element_text(face = "plain", colour = "#000E14"),
        axis.title.x = element_text(face = "plain", colour = "#000E14"),
        panel.grid = element_line(colour = "#D6F1FF"))

(p <- p2 | p1)

ggsave(p, dpi = 300, width = 15, height = 10, units = "in", filename = "2021_week03_kenyacensus.jpeg", device = "jpeg")
