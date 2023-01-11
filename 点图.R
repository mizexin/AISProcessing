# dt=fread("/Volumes/Samsung SSD/AIS/2020EM/636017506.gz",sep = ',')
# dt=fread("/Volumes/Samsung SSD/AIS/2020EM/219018765.gz",sep = ',')
# dt=fread("/Volumes/Samsung SSD/AIS/2020EM/219036000.gz",sep = ',')
dt=fread("/Volumes/Samsung SSD/AIS/2020EM/357104000.gz",sep = ',')
# 219027000

#
# fwrite((dt),paste('/Volumes/Samsung\ SSD/abnormal/SHIP_3.csv',sep = ''),sep = ',',append = T)
library(leaflet)
dt = dt[yday == 312|yday == 313|yday==314|yday == 315]

# Create a color palette with handmade bins.
mypalette <- colorBin( palette="YlOrBr", domain=dt$ME_FC/1000000, na.color="transparent")

# Prepare the text for the tooltip:
mytext <- paste(
  "Time: ", dt$datetime, "<br/>", 
  "Hour: ", dt$durhour, "<br/>", 
  "Distnm: ", dt$distnm, "<br/>", 
  "Speed: ", dt$avgspeed, "<br/>", 
  "AddDay:",dt$isaddedday, "<br/>", 
  "ME_FC: ", dt$ME_FC/1000000, "<br/>", 
 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
m <- leaflet(dt) %>% 
  addTiles()  %>% 
  # setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~lon, ~lat, 
                   fillColor = ~mypalette(ME_FC/1000000), fillOpacity = 0.6, color="white", radius=9, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~ME_FC/1000000, opacity=0.9, title = "FC/t", position = "bottomright" )

m 

# p <- dt %>%
#   ggplot( aes(x = yhour, y = ME_FC/1000000)) +
#   geom_point() +
#   ggtitle("") +
#   theme_ipsum() +
#   ylab("FC") +
#   # geom_label( x=1990, y=55000, label="Amanda reached 3550\nbabies in 1970", size=4, color="#69b3a2") +
#   theme( )
# ggplotly(p)

