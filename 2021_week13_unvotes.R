library(tidyverse)
library(recipes)
library(embed)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(countrycode)
library(lubridate)
library(ggrepel)
library(patchwork)
library(ggtext)


unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


year_col <- roll_calls %>% 
  mutate(year = year(date)) %>% 
  select(rcid, year)

unvotes_year <- left_join(unvotes, year_col, by = "rcid") 
unvotes_year_issue <- left_join(unvotes_year, issues, by = "rcid")

# add in a tag for continent, then use as colour for charts
unvotes_year_issue$continent <- countrycode(sourcevar = unvotes_year_issue$country,
                                            origin = "country.name",
                                            destination = "continent")

# manually add continent for: Czechoslovakia, German Democratic Republic, Yemen Arab Republic, Yemen People's Republic, Yugoslavia, Zanzibar
unvotes_year_issue <- unvotes_year_issue %>% 
  mutate(continent = case_when(!is.na(continent) ~ continent,
                               country == "Czechoslovakia" ~ "Europe",
                               country == "German Democratic Republic" ~ "Europe",
                               country == "Yemen Arab Republic" ~ "Asia",
                               country == "Yemen People's Republic" ~ "Asia",
                               country == "Yugoslavia" ~ "Europe",
                               country == "Zanzibar" ~ "Africa"))

scale_colour_continent <- scale_colour_manual(values = c("#e07a5f", "#81b29a", "#f2cc8f", "#3d405b", "#9a94bc"))


unvotes_ac <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Arms control and disarmament") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

unvotes_co <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Colonialism") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

unvotes_ed <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Economic development") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

unvotes_hr <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Human rights") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

unvotes_nw <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Nuclear weapons and nuclear material") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

unvotes_pc <- unvotes_year_issue %>% 
  select(country, continent, year, issue, rcid, vote) %>%
  filter(issue == "Palestinian conflict") %>% 
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>% 
  select(country, continent, rcid, vote) %>% 
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)


set.seed(30)

# Dimensionality reduction method (UMAP) - mostly taken from Julia Silge's blog post: https://juliasilge.com/blog/un-voting/
umap_rec_ac <- recipe(~., data = unvotes_ac) %>%
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_ac <- prep(umap_rec_ac))

umap_rec_co <- recipe(~., data = unvotes_co) %>% 
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_co <- prep(umap_rec_co))

umap_rec_ed <- recipe(~., data = unvotes_ed) %>% 
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_ed <- prep(umap_rec_ed))

umap_rec_hr <- recipe(~., data = unvotes_hr) %>% 
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_hr <- prep(umap_rec_hr))

umap_rec_nw <- recipe(~., data = unvotes_nw) %>% 
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_nw <- prep(umap_rec_nw))

umap_rec_pc <- recipe(~., data = unvotes_pc) %>% 
  update_role(country, new_role = "country") %>% 
  update_role(continent, new_role = "continent") %>% 
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
(umap_prep_pc <- prep(umap_rec_pc))


p_ac <- bake(umap_prep_ac, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_ac, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_ac, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) +  
  scale_colour_continent + 
  labs(title = "Arms control and disarmament") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))

p_co <- bake(umap_prep_co, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_co, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_co, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) + 
  scale_colour_continent + 
  labs(title = "Colonialism") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))

p_ed <- bake(umap_prep_ed, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_ed, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_ed, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) + 
  scale_colour_continent + 
  labs(title = "Economic development") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))

p_hr <- bake(umap_prep_hr, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_hr, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_hr, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) +  
  scale_colour_continent + 
  labs(title = "Human rights") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))

p_nw <- bake(umap_prep_nw, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_nw, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_nw, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) + 
  scale_colour_continent + 
  labs(title = "Nuclear weapons and nuclear material") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))

p_pc <- bake(umap_prep_pc, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country, colour = continent)) +
  geom_point(data = subset(bake(umap_prep_pc, new_data = NULL), country != "United Kingdom"), alpha = 0.7, size = 2) + 
  geom_point(data = subset(bake(umap_prep_pc, new_data = NULL), country == "United Kingdom"), colour = "#3d405b", fill = "red", size = 2, shape = 24) + 
  geom_text_repel(max.overlaps = Inf, segment.alpha = 0.2, family = "Rubik", size = 6) + 
  scale_colour_continent + 
  labs(title = "Palestinian conflict") + 
  theme(plot.title = element_text(family = "Rubik", face = "bold", hjust = 0.5, size = 70))



p <- ((p_ac | p_co) / (p_ed | p_hr) / (p_nw | p_pc)) + 
  plot_annotation(title = "<span style='font-size:120pt'>UN voting patterns on important issues across 
  **<span style='color:#e07a5f;'>Africa</span>**, 
  **<span style='color:#81b29a;'>America</span>**, 
  **<span style='color:#f2cc8f;'>Asia</span>**, 
  **<span style='color:#3d405b;'>Europe</span>**, and 
  **<span style='color:#9a94bc;'>Oceania</span>**, 1946-2019.
  </span>",
                  caption = "Plots of first two UMAP components. The United Kingdom is highlighted as a red triangle.\nSource: Harvard Dataverse | Visualisation: @Andy_A_Baker",
                  theme = theme(plot.margin = margin(10, 10, 10, 10),
                                plot.title = element_textbox_simple(family = "Rubik", face = "bold", lineheight = 0.3),
                                plot.caption = element_text(family = "Rubik", face = "plain", size = 40, lineheight = 0.3))) & 
  theme(legend.position = "none",
        text = element_text(family = "Rubik", size = 40),
        panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E7E7EF"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


ggsave(p, dpi = 300, width = 15, height = 20, units = "in", filename = "2021_week13_unvotes.jpeg", device = "jpeg")
