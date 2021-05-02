library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(viridis)
library(showtext)
font_add_google("Rubik")
showtext_auto()


# Data prep ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)
departures <- tuesdata$departures

d_notes <- departures %>% 
  select(departure_code, notes, ceo_dismissal) %>% 
  filter(!is.na(departure_code),
         departure_code %in% 1:6) %>% 
  mutate(departure_code = factor(case_when(
    departure_code == 1 ~ "Death",
    departure_code == 2 ~ "Illness",
    departure_code == 3 ~ "Dismissed for job performance",
    departure_code == 4 ~ "Dismissed for legal violations/concerns",
    departure_code == 5 ~ "Retired",
    departure_code == 6 ~ "New career driven succession"), 
    ordered = TRUE))


# Simple text analyses ----------------------------------------------------
# Great tutorial here: https://juliasilge.com/blog/netflix-titles/

other_words <- data.frame(word = c("chairman",
                                   "executive",
                                   "officer",
                                   "ceo",
                                   "board",
                                   "president",
                                   "founder",
                                   "chief",
                                   "mr",
                                   "directors",
                                   "inc",
                                   "corporation",
                                   "company",
                                   "company's",
                                   "also",
                                   "director",
                                   "founder",
                                   "sec",
                                   "corp",
                                   "announced",
                                   "years",
                                   "year",
                                   "co",
                                   "said",
                                   1:10))

d_notes %>% 
  unnest_tokens(word, notes) %>% 
  anti_join(get_stopwords()) %>% 
  anti_join(other_words) %>% 
  count(departure_code, word, sort = TRUE) %>% 
  group_by(departure_code) %>% 
  slice_max(n, n = 15, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, departure_code)) %>% 
  ggplot(aes(n, word, fill = log(n))) + 
  geom_col(show.legend = FALSE, alpha = 0.8) + 
  scale_y_reordered() + 
  scale_fill_viridis() + 
  facet_wrap(~departure_code, scales = "free") + 
  labs(
    x = "Word frequency", y = NULL, 
    title = "Frequently used words* in description by reason for CEO departure", 
    caption = "*Stop words and other irrelevent words removed.
    \nSource: Gentry et al. | Visualisation: @Andy_A_Baker"
  ) + 
  theme_minimal() + 
  theme(text = element_text(family = "Rubik", size = 40),
        plot.title = element_text(family = "Rubik", face = "bold", 
                                  colour = "black", size = 85, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 50),
        strip.text = element_text(face = "bold", size = 40),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(lineheight = 0.2, size = 40),
        plot.margin = margin(10, 10, 10, 10))


ggsave(dpi = 300, width = 14, height = 10, units = "in", 
       filename = "2021_week18_ceodepartures.jpeg", device = "jpeg")
