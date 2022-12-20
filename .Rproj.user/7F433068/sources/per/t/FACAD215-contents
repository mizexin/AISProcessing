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
library(doParallel)
library(circular)

#save as individual ship with stops. remove duplicated points
distance<-function(lon1,lat1,lon2,lat2){
  
  radlat1=rad(lat1);
  radlat2=rad(lat2);
  delta_lon=rad(lon2-lon1);
  top1=cos(radlat2)*sin(delta_lon);
  top2=cos(radlat1)*sin(radlat2)-sin(radlat1)*cos(radlat2)*cos(delta_lon);
  top=sqrt(top1*top1+top2*top2);
  bottom=sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(delta_lon);
  delta_sigma=atan2(top,bottom);
  distance=delta_sigma*6378137.0;
  return (distance);
  
}

addDateTime<-function(aship){
  
  aship=mutate(aship,datetime=as.POSIXct(time,tz='UTC',origin='1970-01-01'))
  return(aship)
  
}

dir = "~/AIS/2019_year_container" 
file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num <- length(file_list)
cl=makeCluster(10)
registerDoParallel(cl)

x<-foreach(i=1:file_num,.packages=c('data.table','dplyr','lubridate','circular')) %dopar% { 
  agroup=fread(paste(file_list[i],sep = ''),
               col.names = c('mmsi','time','status','lon','lat','speed','cog','head'))
  mmsis=agroup[,.N,mmsi]$mmsi
  k=length(mmsis)
  if(nrow(agroup) == 1)
    next
  for (j in seq(1,k))
  {
    print(paste('j=',j))
    ammsi=mmsis[j]
    aship=agroup[mmsi==ammsi]
    aship=distinct(aship)
    aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon=lon/1000000,lat=lat/1000000,sod=speed,cog,head,datetime)]
    aship=aship[,hour:=lubridate::hour(datetime)][,day:=lubridate::day(datetime)][,week:=lubridate::week(datetime)][,month:=lubridate::month(datetime)][,year:=lubridate::year(datetime)][,yday:=yday(datetime)]
    aship=aship[lon<=180][sod>=0][,lon:=as.numeric(lon)][,lat:=as.numeric(lat)]
    aship=aship[,yhour:=hour+(yday-1)*24]
    aship=aship[,dur:=shift(datetime,-1)-datetime][,dur:=as.integer(dur)]
    aship=aship[,lon2:=shift(lon,-1)][,lat2:=shift(lat,-1)][!is.na(lon2)][,speedid:=round(as.numeric(sod))]#
    aship=aship[,dist:=round(distance(lon,lat,lon2,lat2))]
    ship0=aship[,list(.N,hours=round(sum(dur)/3600,2),distnm=round(sum(dist)/1852,2)),list(mmsi,speed=speedid,hour,yhour,day,week,month,year)]
    fwrite(cbind(ship0),paste('~/AIS/2019-stop-result/',ammsi,'.csv',sep = ''))
  }
}
stopCluster(cl)