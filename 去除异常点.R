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
library(isotree)
library(Rlof)
AIS_test <- fread("~/AIS/2019_year_container/ 255806091 .csv",showProgress = T, nThread = 12)%>%arrange(time)
# 255806091 419001218 341456000(扎堆) 566908000（独立点）
AIS_test$time <- as.POSIXct(AIS_test$time, origin="1970-01-01")
AIS_test$lat <- AIS_test$lat/1000000
AIS_test$lon <- AIS_test$lon/1000000


temp = subset(AIS_test,select = c('lon','lat'))
temp = unique(temp)
# test <- as.matrix(temp)
# qplot(temp$lon,temp$lat)
Lof方法识别异常值

test = dbscan::lof(temp,minPts = 10)
temp$dbscan_lof_score = round(test,2)

# 方法二 
# test = temp[,yesno:=DB(dataset = temp, d=1, fraction=0.001)$classification]
# 方法三 
# lof_score = Rlof::lof(temp[,c('lon','lat')], k = 10, cores = 10)
# temp$lof_score = round(lof_score,2)
# 分数差 暂时不用
# plot(density(lof_score))
# temp = temp[order(-temp$lof_score),]
# temp =temp[,lof_score2:=shift(lof_score,-1)][,score_gap:=lof_score-lof_score2]
# Final Map

mypalette <- colorBin(palette="YlOrBr", domain=temp$dbscan_lof_score)
mytext <- paste(
"Lof_score: ",temp$dbscan_lof_score, "<br/>", sep="") %>%
  lapply(htmltools::HTML)

leaflet(temp) %>% 
  addTiles()  %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~lon, ~lat, label = mytext,
                   fillOpacity = 0.9, color="white", radius=5, stroke=FALSE)




temp$scorecut = cut_width(lof_score,2)
ggplot(temp,aes(x = lon, y = lat,colour = lof_score))+
  geom_point(aes(shape = scorecut))+
  ggtitle('2')





r = isolation.forest(test,ntrees = 100, nthreads = 1)
pred <- predict(r, test)

cat("Point with highest outlier score: ",
    test[which.max(pred), ], "\n")
mean(pred)
x = pred

ggplot() +
  geom_point( data=temp, aes(x=lon, y=lat)) 



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

setkey(ais,mmsi,time)
p1=ggplot(ais)+geom_path(aes(x=lon,y=lat))+geom_point(aes(x=lon,y=lat),color='red',alpha=0.3,size=1)+theme_bw(base_size = 24)
ais=ais[,dur:=shift(time,-1)-time][,dur:=as.integer(dur)]  
ais=ais[,lon2:=shift(lon,-1)][,lat2:=shift(lat,-1)]
ais=ais[,dist:=round(distance(lon,lat,lon2,lat2))]
ais=ais[,avgspeed:=round(dist/1852*3600/dur,2)][,speedrate:=avgspeed/20]#5海里以上 超过两倍的平均航速

ab=ais[(dist>5*1852&speedrate>2)]
