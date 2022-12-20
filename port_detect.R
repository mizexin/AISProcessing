library(plyr)
library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(maps)
library(fpc)
library(factoextra)
library(leaflet)
library(plotly)
#AIS文件整合
# dir = "~/AIS/2020_year_container" 
# file_list = list.files(path = dir, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)  
# store_csv = paste(dir,"new.csv")         
# for(i in 1:length(file_list)) 
# {
# df = fread(file = file_list[i],encoding = 'UTF-8', nThread = 12,verbose = T)
# fwrite(df,file = store_csv,append = TRUE)              
# }

# dir = "~/AIS/2020_year_container" 
# file_list = list.files(path = dir, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)  
# for(i in 1:length(file_list)) 
# {
# df = fread(file = file_list[i],encoding = 'UTF-8', nThread = 12,verbose = T)
# fwrite(df,file = store_csv,append = TRUE)              
# }

AIS_test <- fread("~/AIS/2019_year_container/ 518955000 .csv",showProgress = T, nThread = 12)%>%arrange(time)
AIS_test$time <- as.POSIXct(AIS_test$time, origin="1970-01-01")
AIS_test$lat <- AIS_test$lat/1000000
AIS_test$lon <- AIS_test$lon/1000000


test <- subset(AIS_test,select = c('lon','lat'))
  db <- fpc::dbscan(test, eps = .01, MinPts = 8 )
fviz_cluster(db, data = test, stand = FALSE,
             ellipse = T, show.clust.cent = T,pointsize =1,outlier.color = "gray",outlier.pointsize = 0.1,
               geom = "point",shape = 1,palette = "jco", ggtheme= theme_classic())



m <- leaflet()%>%
  addTiles()%>%
  addPolylines(data=test,~lon,~lat,color="red",opacity = 0.7,weight = 1)%>%
  addProviderTiles("Stadia.Outdoors")
ggplotly(m)
attach(test)
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="gray", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )
lines(x=lon, y=lat, col="red", cex=0.01, pch=20)
detach(test)

ggplot()



mydata1_csv <- subset(mydata_csv, my_vessel_type == 'container_ship')
mydata2_csv <- data.frame(mydata1_csv)
mydata2_csv <- na.omit(mydata2_csv)
temp <- as.numeric(mydata2_csv$MMSI)
mydata2_csv[,2] <- temp

# AIS数据遍历匹配MMSI，提取写入
dir = "~/AIS/202007" 
file_list = list.files(path = dir, pattern = "*compress", recursive = T, full.names = T)  
# 新文件路径：dir的上级目录
NEW_AIS_Filelist = paste(dir, "AIS_container_ship.csv")  
for(i in 1:length(file_list)) 
{
  df = fread(file = file_list[i],encoding = 'UTF-8',showProgress = T, nThread = 12,verbose = T)
  df = vroom::vroom(file = file_list[i],num_threads = 12)
  # colnames(df) = c('MMSI','ACQTIME','TARGET_TYPE','DATA_SUPPLIER','DATA_SOURCE','STATUS','LONGITUDE','LATITUDE','AREA_ID','SPEED','CONVERSION','COG','TRUE_HEAD','POWER','LMITATOR','EXTENT')
  colnames(df)[1] <- 'MMSI'
  both <- merge(df, mydata2_csv)
  write_csv(both, file = NEW_AIS_Filelist, append = T, col_names = F)      
  rm(df)
  rm(both)
  gc()
}

mydata_csv <- fread("~/AIS/AIS_container_ship.csv",showProgress = T, nThread = 12)
# colnames(mydata_csv) = c('MMSI','ACQTIME','TARGET_TYPE','DATA_SUPPLIER','DATA_SOURCE','STATUS','LONGITUDE','LATITUDE','AREA_ID','SPEED','CONVERSION','COG','TRUE_HEAD','POWER','LMITATOR','EXTENT','IMO','my_vessel_type','vessel_name_original')

