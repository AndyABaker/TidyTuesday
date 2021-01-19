library(tidyverse)
library(viridis)
library(ggridges)
library(patchwork)
library(graphics)
library(grDevices)

# read in data and join
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

artists <- artists %>% rename(artistId = id)
artwork <- artwork %>% select("artistId", "id", "accession_number", "artist", "artistRole", "title", "dateText", "medium", "creditLine", "year", "acquisitionYear", "dimensions", "width", "height", "depth", "units", "inscription", "thumbnailCopyright", "thumbnailUrl", "url")
art <- full_join(artwork, artists, by = "artistId")
art <- art %>% mutate(area = height*width,
                      age_at_creation = year-yearOfBirth,
                      to_acq_years = year-acquisitionYear,
                      age = yearOfDeath-yearOfBirth,
                      ratio_wh = width/height,
                      ratio_hw = height/width,
                      ratio = if_else(width>height, width/height, height/width),
                      century = case_when(year <= 1800 ~ "16th to 18th century",
                                          year > 1800 & year <= 1900 ~ "19th century",
                                          year > 1900 & year <= 2100 ~ "20th to 21st century"))

# sample just to testing plot quickly
# art <- art[sample(nrow(art), 3000), ]

# plots
(p1 <- art %>% 
  filter(!is.na(century), century != 1500) %>%
  ggplot(aes(width, height, colour = century)) +
  geom_point(alpha = 0.05) + 
  scale_x_log10() + 
  scale_y_log10() + 
  scale_colour_manual(values = c("#062E37", "#0E6C81", "#16A9CA")) + 
  theme_minimal() + 
  labs(x = "Width (mm)",
       y = "Height (mm)",
       caption = "Source: Tate Collection | Visualisation: @Andy_A_Baker") + 
  coord_fixed(ratio = 1, clip = "off") + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#EDFAFD", colour = "#EDFAFD"),
        plot.background = element_rect(fill = "#EDFAFD"),
        text = element_text(family = "ITC Officina Sans"),
        axis.text = element_text(face = "plain", colour = "#020F12"),
        axis.title.y = element_text(face = "bold", colour = "#020F12"),
        axis.title.x = element_text(face = "bold", colour = "#020F12"),
        strip.text.x = element_text(face = "bold", colour = "#020F12")))

(p2 <- art %>% filter(!is.na(century),
               ratio < 2.5) %>% 
  ggplot(aes(x = ratio, y = century, colour = century, fill = century)) + 
  geom_density_ridges2(alpha = 0.5, scale = 0.95, stat = "binline", bins = 150) + 
  geom_vline(aes(xintercept = 1.61803398875), colour = "#F3B61F") + 
  annotate(
    geom = "curve", x = 1.75, y = 3.38, xend = 1.62, yend = 3.4, 
    curvature = 0.2, arrow = arrow(length = unit(2, "mm")), 
    colour = "#F3B61F"
    ) +
  annotate(geom = "text", x = 1.76, y = 3.38, label = "The Golden Ratio", hjust = "left", colour = "#F3B61F") + 
  scale_colour_manual(values = c("#062E37", "#0E6C81", "#16A9CA")) + 
  scale_fill_manual(values = c("#062E37", "#0E6C81", "#16A9CA")) + 
  coord_cartesian(clip = "off") +
  labs(x = "Aspect ratio (longest side / shortest side)",
       y = "") + 
  scale_y_discrete(expand = c(0,-1.5)) + 
  scale_x_continuous(breaks = seq(from = 1, to = 2.5, by = 0.25)) + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#EDFAFD", colour = "#EDFAFD"),
        plot.background = element_rect(fill = "#EDFAFD"),
        text = element_text(family = "ITC Officina Sans"),
        axis.text.x = element_text(face = "plain", colour = "#020F12"),
        axis.text.y = element_text(face = "bold", size = 11, colour = "#020F12"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", colour = "#020F12")))

p <- (p2 | p1)
p

# save
ggsave(p, dpi = 300, width = 20, height = 10, units = "in", filename = "2021_week03_artcollections.jpeg", device = "jpeg")
