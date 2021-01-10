library(tidyverse)
library(viridis)
# devtools::install_github("easystats/correlation")
library(correlation)
library(see)
library(ggraph) 
library(countrycode)
library(ggtext)
library(scales)
library(extrafont)
library(ggthemes)
# font_import()
# loadfonts(device = "win")
library(ggExtra)
library(ggrepel)

# Read in data and format
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
transit_cost$cost_km_millions <- as.numeric(transit_cost$cost_km_millions)
transit_cost$start_year <- as.numeric(transit_cost$start_year)
transit_cost$end_year <- as.numeric(transit_cost$end_year)

# add continent variable
transit_cost <- countrycode::codelist %>%
  select(country.name.en, iso2c, continent) %>%
  right_join(transit_cost, by = c("iso2c" = "country"))

# add construction time and text labels
transit_cost <- transit_cost %>% 
  mutate(construction_time = end_year-start_year,
         label_text = paste0(line,", ",city,"\n","$",round(cost_km_millions, digits = 0)," per km"), sep = "")

# reproducible plot (jittered as construction_time is in distinct years)
pos <- position_jitter(width = 0.5, seed = 12345)
p <- transit_cost %>% 
  filter(!is.na(country.name.en),
         !is.na(continent),
         !is.na(cost_km_millions),
         !is.na(construction_time),
         continent %in% c("Americas", "Asia", "Europe")) %>% 
  ggplot(aes(construction_time, length, 
             size = cost_km_millions, 
             colour = continent, 
             fill = continent, 
             label = ifelse(e %in% c("7411", "7241", "8193"), label_text, ""))) + 
  geom_point(alpha = 0.5, 
             position = pos
             ) + 
  scale_y_log10(breaks = c(1,10,100)) + 
  scale_color_viridis_d(end = 0.99) + 
  scale_fill_viridis_d(end = 0.95) + 
  theme_minimal() + 
  labs(title = "Transit Costs",
       subtitle = "<span style='font-size:11pt'>Transit lines in 
**<span style='color:#238A8DFF;'>Asia</span>**, 
**<span style='color:#e3cd02FF;'>Europe</span>** and the
**<span style='color:#440154FF;'>Americas</span>** vary wildly in length, contruction time and cost per km.
  </span>",
       caption = "Source: Transit Costs Project | Visualisation: @Andy_A_Baker",
       y = "Line length (km)",
       x = "Construction time (years)") + 
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "ITC Officina Sans"),
        plot.title = element_text(face = "bold", vjust = -0.5), 
        plot.subtitle = element_textbox_simple(lineheight = 1.1, vjust = -0.8),
        axis.text = element_text(face = "plain"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold")) + 
  geom_text_repel(
    size = 4,
    family = "ITC Officina Sans",
    point.padding = 10,
    # nudge_x = 0.2,
    # nudge_y = -0.2,
    segment.linetype = 1,
    segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc")),
    position = pos
  )

# add marginal density plots
(p <- ggMarginal(p, groupColour = T, groupFill = T))

# save
ggsave(p, dpi = 300, width = 10, height = 10, units = "in", filename = "2021_week01_transitcosts.jpeg", device = "jpeg")
