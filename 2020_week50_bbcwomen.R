library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(cowplot)
library(webshot)
library(htmlwidgets)
library(magick)

# to fix wordcloud2 issues
# library(devtools)
# library(jsonlite)
# library(htmlwidgets)
# library(rlang)
# devtools::install_github("lchiffon/wordcloud2")

# read in data
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')


# pull out description text and save as corpus
desc <- women$description
desc_c <- Corpus(VectorSource(desc))


# clean up text
desc_c <- desc_c %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
desc_c <- tm_map(desc_c, content_transformer(tolower))
desc_c <- tm_map(desc_c, removeWords, stopwords("english"))
desc_c <- tm_map(desc_c, removeWords, c("name"))
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
desc_c <- tm_map(desc_c, removeSpecialChars)

dtm <- TermDocumentMatrix(desc_c) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix), decreasing = TRUE) 
df <- data.frame(word = names(words), freq = words)


# wordcloud
df %>% group_by(freq) %>% summarise(count = n())
df <- df %>% mutate(pallete_a = cut(freq, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,13,17),
                                    labels = c("#ECE7F2", "#ECE7F2", "#D0D1E6", "#A6BDDB", "#A6BDDB", "#74A9CF", "#74A9CF", "#3690C0", "#3690C0", "#0570B0", "#0570B0", "#045A8D", "#023858")))
df %>% case_when(pallete_a == "NA"~ "#ECE7F2", TRUE ~ as.character(pallete_a))
# https://freesvg.org/female-head-profile-silhouette-image
wordcloud <- wordcloud2(df, 
                        figPath = "C:/Users/Andy/OneDrive/R code/TidyTuesday/woman_profile.png", 
                        minRotation = 0,
                        maxRotation = 0,
                        size = 0.5,
                        color = df$pallete_a)

saveWidget(wordcloud, "tmp.html")
webshot("tmp.html", "wordcloud.png", delay = 5, vwidth = 693, vheight = 777) # this seemed to sometimes fail and just save a blank image, unsure why.


# make title/text bit
(p <- ggplot() + 
    draw_image("wordcloud_m.png", x = 4.9, y = 4, scale = 9) + # had to manually screenshot image, as webshot failed.
    geom_text(aes(x = 5, y = 10, label = "The BBC's 100 Women of 2020", fontface = "bold"), 
              colour = "#023858", size = 10, family = "Helvetica") +
    geom_text(aes(x = 5, y = 9.2, label = "The BBC has revealed its list of 100 inspiring and influential women from around the world for 2020. This year\n 100 Women is highlighting those who are leading change and making a difference during these turbulent times. \nHere's how they were most often described.", fontface = "bold"), 
              colour = "#045A8D", size = 4, family = "Helvetica") +
    geom_text(aes(x = 5, y = 1, label = "Source: BBC | Visualisation: @Andy_A_Baker"),
              colour = "#045A8D", size = 4)+
    xlim(0, 10)+
    ylim(1, 10)+
    theme(plot.background = element_rect(fill = "white", colour = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()))


# Save plot
ggsave(p, dpi = 100, width = 9, height = 10, units = "in", filename = "2020_week50_bbcwomen.jpeg", device = "jpeg")
