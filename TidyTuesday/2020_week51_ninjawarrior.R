library(tidyverse)
library(data.table)
library(devtools)
# devtools::install_github('gastonstat/arcdiagram')
library(arcdiagram)
library(gcookbook)
library(igraph)
library(cowplot)

nj <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')
nj$obstacle_name[nj$obstacle_name == "Warped Wall / Mega Wall"] <- "Warped Wall"

nj_f <- nj %>% 
  filter(round_stage == "Qualifying (Regional/City)") %>% 
  group_by(obstacle_name) %>% 
  mutate(count = n()) %>% 
  #filter(count > 1) %>% 
  arrange(count)

order_pop <- nj %>% 
  filter(round_stage == "Qualifying (Regional/City)") %>% 
  group_by(obstacle_name) %>% 
  mutate(count = n()) %>% 
  arrange(count) %>% 
  select(count, obstacle_name)
order <- unique(order_pop$obstacle_name)


nj_f <- setDT(nj_f)[, CJ(Ind1 = obstacle_name, Ind2 = obstacle_name, unique = TRUE)[Ind1 != Ind2], 
                    location][!duplicated(data.table(pmax(Ind2, Ind1), pmin(Ind2, Ind1)))]
nj_f <- nj_f %>%
  select(-1)

m <- nj_f[1:nrow(nj_f) %% 2 == 1, ]
g <- graph.data.frame(m, directed=FALSE)

nj_f.edges <- get.edgelist(g)

png(filename="arc_nj.png", width = 1000, height = 1000, units = "px")
op = par(mar = c(0.1, 15, 0.1, 0.5))
arcplot(nj_f.edges, 
        ordering = order,
        horizontal = FALSE,
        sorted = TRUE, 
        decreasing = TRUE,
        lwd.arcs = 4*runif(10, 0.5, 2), 
        col.arcs = hsv(runif(3, 0, 0), alpha = 0.2))
dev.off()
