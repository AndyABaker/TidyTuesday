library(tidyverse)
library(forcats)
library(patchwork)

# read in data and set NAs as zeros
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
plastics[is.na(plastics)] <- 0

# tidy, some company names were duplicated
'%!in%' <- function(x,y) !('%in%'(x,y))
plastic_types <- plastics %>%
  filter(parent_company %!in% c("Grand Total", "Unbranded", "null", "NULL")) %>%
  select(-7) %>% 
  mutate(parent_company = if_else(parent_company == "Nestle", "Nestlé", parent_company),
         parent_company = if_else(parent_company == "Pepsico", "PepsiCo", parent_company),
         parent_company = if_else(parent_company == "Procter & Gamble", "P&G", parent_company),
         parent_company = if_else(parent_company == "Tamil Nadu Co-operative Milk Producers' Federation Ltd", "TCMPF", parent_company)) %>% 
  gather(key = "type", value = "count", hdpe:pvc) %>% 
  select(parent_company, type, count) %>% 
  group_by(parent_company) %>% 
  mutate(company_total_count = sum(count)) %>% 
  group_by(parent_company, type) %>%
  mutate(company_type_count = sum(count)) %>% 
  select(-count) %>% 
  distinct()

# prepare angles of labels https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
label_data <- plastic_types %>% filter(company_total_count > 1500) %>% group_by(parent_company) %>% summarize(value = mean(company_total_count)) %>% arrange(value)
label_data <- label_data %>% mutate(id = row_number())
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle + 180, angle)

# plot
(p <- plastic_types %>% 
    filter(company_total_count > 1500) %>% 
    ggplot(aes(reorder(parent_company, company_total_count), company_type_count, fill = type)) + 
    geom_bar(stat = "identity", alpha = 0.7) + 
    scale_fill_manual(values = c("#95ac67", "#d2cc91", "#7c392f", "#8dbbe2", "#4c81c4", "#a27eb2")) + 
    coord_polar(start = 0) + 
    ylim(-5000, 30000) + 
    labs(fill = "Plastic type") + 
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.43)) + 
    theme_minimal() +
    theme(
      legend.position = c(0.5, 0.2),
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "#f0efeb", colour = NA),
      plot.background = element_rect(fill = "#f0efeb", colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) + 
    labs(caption = "Source: Break Free from Plastic | Visualisation: @Andy_A_Baker") + 
    geom_text(data = label_data, aes(x = id, y = value + 200, label = parent_company, hjust = hjust), 
              color = "black", 
              fontface = "bold", 
              family = "ITC Officina Sans",
              alpha = 0.6, 
              size = 2.5, 
              angle = label_data$angle, 
              inherit.aes = FALSE))


ggsave(p, dpi = 300, width = 12, height = 12, units = "in", filename = "2021_week05_plasticpollution.jpeg", device = "jpeg")
