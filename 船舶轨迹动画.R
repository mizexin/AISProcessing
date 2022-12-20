library(data.table)
library(fs)
library(mapdeck)
library(dplyr)
library(tmap)
library(mapdeck)
source('/Users/mizexin/R/AIS_Processing/functions2020-copy.R')
mapbox_key='pk.eyJ1IjoiaGl0d3poIiwiYSI6ImNqcDI1b2o5dzAzb2Qza25yMWhqNjFjZHQifQ._Dph5Y7NZqMaL4qSAN8VPg'
dt = fread('/Volumes/Samsung\ SSD/single.csv')
dt = dt[yday == 30]
dt1=dt[,X:=lon][,Y:=lat][,Z:=ME_FC][,M:=time]
setkey(dt1,time)

sfc <- sfheaders::sf_linestring(
  obj = dt1
  , x = "lon"
  , y = "lat"
  , z="speed"
  , m='time'
  , linestring_id = 'mmsi'
  
)

mapdeck(token = mapbox_key,style = mapdeck_style("dark"), pitch = 5) %>%
  add_hexagon(
    data = dt
    , lat = "lat"
    , lon = "lon"
    , layer_id = "hex_layer"
    , elevation_scale = 1000
    , colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
  )%>%
  add_trips(
    
    data = sfc,
    stroke_colour ="#cc3300",
    stroke_width = 10000,
    opacity = 0.3,
    palette = "viridis",
    trail_length = 180000,
    start_time = min(dt1$time),
    end_time = max(dt1$time),
    animation_speed = 10000
  )

