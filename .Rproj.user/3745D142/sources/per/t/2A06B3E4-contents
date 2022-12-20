#identify stops of an individual ship
library(ggplot2)
# library(tmap)
library(readxl)
library(hrbrthemes)
library(lubridate)
hrbrthemes::import_roboto_condensed()
library(mapdeck)
library(data.table)
library(dplyr)
library(sf)
library(mapview)
library(extrafont)
library(tidycovid19)
library(data.table)
library(foreach)
library(doSNOW)
library(circular)

#save as individual ship with stops. remove duplicated points
source('~/R/AIS_Processing/functions2020-copy.R')
dir = "~/AIS/temp" 
file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num <- length(file_list)
cl=makeCluster(1)
registerDoSNOW(cl)

pb <- txtProgressBar(min = 1, max = 1000, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

x<-foreach(i=1:file_num,.packages=c('data.table','dplyr','lubridate','circular'), .options.snow=opts) %dopar% { 
  agroup=fread(paste(file_list[i],sep = ''),
               col.names = c('mmsi','time','status','lon','lat','sog','cog','head'))
  mmsis=agroup[,.N,mmsi]$mmsi
  k=length(mmsis)
  #get stops
  if(nrow(agroup) == 1)
    next
  for (j in seq(1,k))
  {
    #j=3 
    print(paste('j=',j))
    ammsi=mmsis[j]
    aship=agroup[mmsi==ammsi]
    aship=distinct(aship)#remove duplicated points
    setkey(aship,mmsi,time)
    aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon=lon/1000000,lat=lat/1000000,sog=sog/10,cog,head,datetime)]
    aship=aship[,hour:=hour(datetime)][,day:=day(datetime)][,month:=month(datetime)][,sogknot:=floor(sog/10)]
    aship_stops=detectShipStops(aship);
    aship_stops=aship_stops[,hour:=hour(datetime)][,day:=day(datetime)][,month:=month(datetime)][,sogknot:=floor(sog/10)]
    lines=setPoints(aship_stops)%>%setLines(.)%>%addLineSpeed(.)
    fwrite(cbind(lines),paste('~/AIS/2019-stop-result/',ammsi,'.csv',sep = ''))
  }
}
close(pb)
stopCluster(cl)