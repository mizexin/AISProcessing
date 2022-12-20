getwd()
setwd("C:/Users/zxs/Desktop")
getwd()

package.list=c("geoviz","tidyverse","sf","terra","rasterVis","ggspatial",
               "rgdal","rnaturalearth","rnaturalearthdata","raster")

for (package in package.list) {
  if(!require(package,character.only=T,quietly=T)){
    install.packages(package)
    library(package,character.only = T)
  }
  
} 

shp1 <- sf::read_sf("zhejiang.json")
p1<-ggplot()+
  geom_sf(data=shp1,aes(fill=NULL))+
  annotation_scale(location="br")+# 设置距离刻度尺
  #annotation_north_arrow(location="tl",style = north_arrow_nautical(
  #  fill=c("grey40","white"),line_col="grey20"
  #))+
  labs(x=NULL,y=NULL)+
  geom_sf(data=shp1,fill="#AFB3B3",size=0.4,color="gray71")+#添加地图边界
  xlim(118,123)+ylim(27,35)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())+
  theme(plot.background = element_rect(fill="#9ECFDD",
                                       color="gray71",size = 0.5))+
  annotation_north_arrow(location="tl",
                         style = north_arrow_fancy_orienteering(
                           fill = c("grey40","white"),
                           line_col = "grey20"))
p1
