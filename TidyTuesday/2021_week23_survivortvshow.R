library(survivoR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()


# Data processing ---------------------------------------------------------

d_cast <- castaways %>% 
    group_by(season, personality_type) %>% 
    summarise(votes = if_else(str_detect(personality_type, "^E"), 
                              sum(total_votes_received), 
                              -sum(total_votes_received)),
              personality_type = factor(personality_type, 
                                        levels = c("INTJ",
                                                   "INTP",
                                                   "INFJ",
                                                   "INFP",
                                                   "ISTJ",
                                                   "ISFJ",
                                                   "ISTP",
                                                   "ISFP",
                                                   
                                                   "ENTJ",
                                                   "ENTP",
                                                   "ENFJ",
                                                   "ENFP",
                                                   "ESTJ",
                                                   "ESFJ",
                                                   "ESTP",
                                                   "ESFP"),
                                        ordered = TRUE)) %>% 
  distinct()

fill_personality <- c("INTJ" = "#b5e48c",
                      "INTP" = "#99d98c",
                      "INFJ" = "#76c893",
                      "INFP" = "#52b69a",
                      "ISTJ" = "#34a0a4",
                      "ISFJ" = "#168aad",
                      "ISTP" = "#1a759f",
                      "ISFP" = "#1e6091",
                      
                      "ENTJ" = "#FFBA08",
                      "ENTP" = "#FAA307",
                      "ENFJ" = "#F48C06",
                      "ENFP" = "#E85D04",
                      "ESTJ" = "#DC2F02",
                      "ESFJ" = "#D00000",
                      "ESTP" = "#9D0208",
                      "ESFP" = "#6A040F")


# Plotting ----------------------------------------------------------------

d_cast %>% ggplot(aes(season, votes, fill = personality_type)) + 
  geom_col() + 
  coord_flip() + 
  annotate(geom = "text", x = 1.04, y = -63.5,
           label = c("Season one"), size = 13, family = "Rubik") + 
  annotate(geom = "curve", x = 1, y = -51, xend = 1, yend = -42, 
           arrow = arrow(length = unit(2, "mm")), curvature = 0) +
  scale_y_continuous(limits = c(-129, 129),
                     labels = abs(seq(-120, 120, 20)),
                     breaks = seq(-120, 120, 20),
                     expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0.01, 0)) + 
  scale_fill_manual(values = fill_personality,
                    breaks = c("INTJ",
                               "INTP",
                               "INFJ",
                               "INFP",
                               "ISTJ",
                               "ISFJ",
                               "ISTP",
                               "ISFP",
                               
                               "ESFP",
                               "ESTP",
                               "ESFJ",
                               "ESTJ",
                               "ENFP",
                               "ENFJ",
                               "ENTP",
                               "ENTJ")) + 
  guides(fill = guide_legend(nrow = 1,
                             label.position = "top",
                             label.hjust = 0.5)) + 
  labs(title = "Votes received by personality type over 40 seasons of Survivor\n",
       y = "Votes received",
       x = NULL,
       fill = NULL,
       caption = "Source: survivoR Package | Visualisation: @Andy_A_Baker") + 
  theme(
    legend.spacing.y = unit(0.2, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.position = c(0.5, 1.05),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.45, "cm"),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_line(colour = "grey95"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(colour = "black", family = "Rubik", size = 40),
    axis.text = element_text(colour = "black", family = "Rubik"),
    plot.title = element_text(face = "bold", family = "Rubik", size = 74,
                              hjust = 0.5, lineheight = 0.55),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(size = 40),
    plot.margin = margin(10, 10, 10, 10))

ggsave(dpi = 300, width = 11, height = 10, units = "in", 
       filename = "2021_week23_survivortvshow.jpeg", device = "jpeg")


# DON'T FORGET ALT TEXT