library(tidyverse)
library(heatwaveR)
library(scales)
library(extrafont)
library(ggthemes)
# font_import()
# loadfonts(device = "win")
library(ggtext)

big_mac <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv")

p <- big_mac %>% filter(name != "United States", !is.na(usd_adjusted)) %>% 
  ggplot(aes(date, usd_adjusted)) + 
  geom_flame(aes(x = date, y2 = usd_adjusted, y = 0), fill = "#e63946") +
  geom_flame(aes(x = date, y = usd_adjusted, y2 = 0), fill = "#52b788") +
  geom_hline(yintercept = 0) + 
  facet_wrap(vars(name), ncol = 6) + 
  theme_minimal() +
  labs(title = "The Big Mac Index",
       subtitle = "<span style='font-size:11pt'>Invented by The Economist in 1986, the big mac index is a lighthearted guide to whether currencies are at their correct level. It is based on the theory of purchasing-power parity (PPP), the notion that in the long run exchange rates should move towards the rate that would equalise the prices of an identical basket of goods and services (in this case, a burger) in any two countries. Here we show how the GDP adjusted index conveys whether each currency is
**<span style='color:#52b788;'>over</span>**- or 
**<span style='color:#e63946;'>under</span>**-valued.
  </span>",
       caption = "Source: The Economist | Visualisation: @Andy_A_Baker",
       y = "GDP adjusted index, relative to the US dollar") + 
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "ITC Officina Sans"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face = "plain"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        strip.text.x = element_text(face = "bold"), 
        plot.subtitle = element_textbox_simple(lineheight = 1.1))

ggsave(p, dpi = 300, width = 10, height = 10, units = "in", filename = "2020_week52_bigmac.jpeg", device = "jpeg")
