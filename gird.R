# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(ggplot2)

# Load dataset from github
data <- read.table("~/AIS/2019-stop-result/564071000.csv", sep=",", header=T)

# Get the world polygon
world <- map_data("world")

# plot
ggplot(data, aes(x=g.lon, y=g.lat)) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_bin2d(bins=100) +
  ggplot2::annotate("text", x = 175, y = 80, label="Container", colour = "black", size=4, alpha=1, hjust=1) +
  theme_void() +
  ylim(-70, 80) +
  # scale_fill_viridis(
  #   trans = "log", 
  #   breaks = c(1,10,20,300),
  #   # name=" ", 
  #   guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  # )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.8, 0.09),
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

