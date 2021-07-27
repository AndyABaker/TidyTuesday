library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(lubridate)
library(janitor)
library(maps)
library(tibbletime)
library(gganimate)


# Inspired by Katie Press's blog post: 
# https://kpress.dev/blog/2021-07-20-tidy-tuesday-drought-conditions/
#https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx
# County, Percent Area, Categorical, CSV
dr_county <- read_csv("dm_export_20190814_20210726.csv") %>% 
  pivot_longer(cols = None:D4, 
               names_to = "drought_lvl", values_to = "area_pct") %>% 
  clean_names() %>% 
  mutate(drought_lvl = factor(
    case_when(drought_lvl == "None" ~ "No drought",
              drought_lvl == "D0" ~ "Abnormally dry",
              drought_lvl == "D1" ~ "Moderate drought",
              drought_lvl == "D2" ~ "Severe drought",
              drought_lvl == "D3" ~ "Extreme drought",
              drought_lvl == "D4" ~ "Exceptional drought"),
    levels = c("Exceptional drought", 
               "Extreme drought", 
               "Severe drought", 
               "Moderate drought", 
               "Abnormally dry", 
               "No drought"))) 

temp_drought <- dr_county %>% 
  group_by(fips, valid_start) %>% 
  arrange(fips, valid_start, desc(area_pct), drought_lvl) %>% 
  slice(1)

data("county.fips")

county.fips <- county.fips %>% 
  mutate(region = word(polyname, 1, sep = ","),
         subregion = word(polyname, 2, sep = ",")) %>% 
  mutate(subregion = word(subregion, 1, sep = ":")) %>% 
  mutate(fips = str_pad(as.character(fips), side = "left", width = 5, "0"))

map_usa <- map_data("county")

map_usa <- map_usa %>% 
  left_join(county.fips)

series <- tibbletime::create_series("2019-08-20" ~ "2021-07-13", "weekly") %>% 
  mutate(join_col = 1) %>% 
  rename("valid_start" = date)

map_usa <- map_usa %>% 
  mutate(join_col = 1) %>% 
  left_join(series)

map_usa <- map_usa %>% 
  left_join(temp_drought)

temp_nas <- map_usa %>% 
  filter(region == "south dakota", subregion %in% c("shannon", "bennett")) %>% 
  arrange(valid_start, subregion)

temp_nas <- temp_nas %>% 
  fill(c(map_date, state:area_pct)) %>% 
  mutate(county = replace_na(county, "Shannon County")) %>% 
  filter(county == "Shannon County")

map_usa <- map_usa %>% 
  filter(fips != "46113") %>% 
  bind_rows(temp_nas) %>% 
  mutate(valid_start = as.Date(valid_start))


# Mapping -----------------------------------------------------------------

map <- map_usa %>% 
  ungroup() %>% 
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = fct_rev(drought_lvl))) + 
  borders("county", colour = "#FFF8EB", lwd = 0.05) + 
  borders("state", colour = "#FFF8EB", lwd = 1) + 
  coord_map() + 
  scale_fill_manual(values = c("#F1F9F6",
                               "#e9d8a6",
                               "#ee9b00",
                               "#ca6602",
                               "#a61e11",
                               "#641619")) + 
  guides(fill = guide_legend(nrow = 1,
                             label.position = "top",
                             label.hjust = 0.5)) + 
  labs(title = "Drought levels across US counties",
       caption = "Week starting: {current_frame}
       Source: Drought Monitor | Visualisation: @Andy_A_Baker",
       fill = NULL) + 
  theme_void() + 
  theme(legend.spacing.y = unit(0.3, "cm"),
        legend.spacing.x = unit(3, "cm"),
        legend.position = c(0.5, 0.98),
        legend.key.width = unit(11, "cm"),
        legend.key.height = unit(0.6, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(10, 10, 10, 10),
        text = element_text(family = "Rubik", size = 30),
        plot.caption = element_text(family = "Rubik", size = 30, hjust = 0.5,
                                    lineheight = 1.2, 
                                    margin = margin(-10, 0, 0, 0)),
        plot.title = element_text(family = "Rubik", face = "bold", size = 70, 
                                  hjust = 0.5, margin = margin(0, 0, 15, 0)),
        panel.background = element_rect(fill = "#FFF8EB", colour = NA),
        plot.background = element_rect(fill = "#FFF8EB", colour = NA),
        panel.border = element_blank(),
        plot.margin = margin(50, 10, 30, 10)) + 
  transition_manual(frames = valid_start)

animate(map, fps = 8, width = 2500, height = 2000, 
        renderer = gifski_renderer(loop = TRUE), end_pause = 15)
anim_save("2021_week30_usdroughts.gif")
