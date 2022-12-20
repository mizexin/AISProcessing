library(tigris)
library(tidyverse)
library(sf)
options(tigris_class = "sf", tigris_use_cache = TRUE)
set.seed(1234)

aa<- sf::read_sf("china.json")%>%
  filter(name == "浙江省") %>%
  st_transform(26910)

g <- aa %>%
  st_make_grid(cellsize = 200000) %>%
  # st_intersection(aa) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())
plot(aa$geometry)

sj <- places("CA", cb = TRUE) %>%
  filter(NAME == "San Jose") %>%
  st_transform(26910)

g <- sj %>%
  st_make_grid(cellsize = 2000) %>%
  st_intersection(sj) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())

thefts <- st_sample(sj, size = 500) %>%
  st_sf()

assaults <- st_sample(sj, size = 200) %>%
  st_sf()

plot(g$geometry)
plot(thefts, add = TRUE, col = "red")

theft_grid <- g %>%
  st_join(thefts) %>%
  group_by(id) %>%
  summarize(num_thefts = n())

plot(theft_grid["num_thefts"])

assault_grid <- g %>%
  st_join(assaults) %>%
  group_by(id) %>%
  summarize(num_assaults = n()) 

st_geometry(assault_grid) <- NULL

crime_data <- left_join(theft_grid, assault_grid, by = "id")

crime_data

filename <- system.file("ex/meuse.tif", package="terra")
filename <- system.file("ex/meuse.tif", package="terra")
filenames<- sf::read_sf("zhejiang.json")
## [1] "C:/soft/R/R-devel/library/terra/ex/meuse.tif"
r <- rast(filename)
plot(filenames)

