library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(Cairo)
library(janitor)
library(recipes)
library(ggtext)


tuesdata <- tidytuesdayR::tt_load(2021, week = 26)
parks <- tuesdata$parks

parks_simple <- parks %>% 
  group_by(city) %>% 
  mutate(rank_latest_calc = ifelse(year == max(year), rank, NA),
         rank_latest = mean(rank_latest_calc, na.rm = TRUE),
         total_pct_latest_calc = ifelse(year == max(year), total_pct, NA),
         total_pct_latest = mean(total_pct_latest_calc, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(med_park_size_data = as.numeric(str_remove(med_park_size_data, "%")),
         park_pct_city_data = as.numeric(str_remove(park_pct_city_data, "%")),
         pct_near_park_data = as.numeric(str_remove(pct_near_park_data, "%")),
         spend_per_resident_data = as.numeric(
           str_remove(spend_per_resident_data, "\\$"))) %>% 
  select(year, total_pct_latest, city, ends_with("_data"))


# PCA ---------------------------------------------------------------------
# Inspired by Allison Horst's work here:
# https://allisonhorst.github.io/palmerpenguins/articles/articles/pca.html


parks_recipe <- recipe(~., data = parks_simple) %>% 
  update_role(year, new_role = "year") %>% 
  update_role(total_pct_latest, new_role = "total_pct_latest") %>% 
  update_role(city, new_role = "city") %>% 
  step_naomit(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>% 
  prep()

parks_pca <- parks_recipe %>% 
  tidy(id = "pca") 


(pct_var <- parks_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance") %>% 
  mutate(component = paste0("PC", component, sep = ""),
         value_pct = value) %>% 
  select(component, value_pct))


parks_pca %>% 
  left_join(pct_var) %>% 
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
  mutate(
    terms = tidytext::reorder_within(terms, abs(value), component),
    component = paste0(component, " (", round_half_up(abs(value_pct), 1), "%)")
  ) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(x = "Absolute value of contribution",
       y = NULL,
       fill = "Positive?")


# Biplot ------------------------------------------------------------------

pca_wider <- parks_pca %>% 
  tidyr::pivot_wider(names_from = component, id_cols = terms) %>% 
  mutate(term_lab = c("Median park size",
                      "Parkland as proportion of city (%)",
                      "Residents within a 10 minute walk (%)",
                      "Spending per capita ($)",
                      "Basketball hoops per capita",
                      "Dog parks per capita",
                      "Playgrounds per capita",
                      "Recreation & senior centers per capita",
                      "Restrooms per capita",
                      "Splashgrounds per capita"),
         dog = ifelse(terms == "dogpark_data", -2.8, 1))

juice(parks_recipe) %>% 
  ggplot(aes(PC1, PC2)) + 
  geom_segment(data = pca_wider,
               aes(xend = 10.2 * PC1, yend = 10.2 * PC2), 
               x = 0, 
               y = 0, 
               alpha = 1,
               size = 0.9,
               lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(.1, "inches"), type = "closed"),
               color = "#3D405B") + 
  geom_line(aes(group = city, color = total_pct_latest), 
            lineend = "round", linejoin = "round",
            arrow = arrow(length = unit(.05, "inches"), type = "closed")) + 
  geom_text(data = pca_wider,
            aes(x = 6.8 * PC1, y = 6.8 * PC2, 
                label = term_lab,
                angle = (atan(PC2/PC1)/pi)*180,
                vjust = -0.5*dog), 
            hjust = 0.5,
            size = 10, 
            alpha = 0.9,
            color = "#3D405B") + 
  annotate(geom = "text", x = 4.5, y = -5.5, hjust = 1, lineheight = 0.35,
           colour = "#3D405B",
           family = theme_get()$text[["family"]], 
           size = theme_get()$text[["size"]],
           label = "Biplot of the first two principal components from the PCA.
           Data is limited to 713 major US cities, from 2012 to 2020.
           Source: TPL | Visualisation: @Andy_A_Baker") + 
  scale_colour_gradient2(low = "#D85531",
                         mid = "#F2CC8F",
                         high = "#538D22",
                         midpoint = 50,
                         guide = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(title = "The evoution of parks in US cities",
       subtitle = "Each small arrow indicates changes in a city's 
       characteristics over time.<br>
       Colour indicates the city's latest ParkScore® Index, ranging from 
       **<span style='color:#D85531;'>low</span>** to 
       **<span style='color:#538D22;'>high</span>**.",
       colour = "Total points (%)") + 
  theme_void() + 
  coord_fixed() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        text = element_text(colour = "#3D405B", family = "Rubik", size = 35),
        plot.title = element_text(face = "bold", size = 85, hjust = 0.5,
                                  margin = margin(0, 0, 5, 0)),
        plot.subtitle = element_textbox_simple(family = "Rubik", size = 40,
                                               lineheight = 0.35, halign = 0.5),
        plot.margin = margin(10, 0, 10, 0))

ggsave(dpi = 300, width = 10, height = 10, units = "in", type = "cairo",
       filename = "2021_week26_publicparkaccess.jpeg", device = "jpeg")
