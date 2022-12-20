library(RODBC)
library(foreach)
library(data.table)
library(lubridate)
library('dplyr')
library('stringr')
library('ggplot2')
library('ggthemes')
library(ggpubr)

rad<-function(d){  
  return (d * pi/180);   
}

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

addDateTime<-function(aship){#aship has timestamp column as time, timezone is set to UTC,use lubridate package
  
  aship=mutate(aship,datetime=as.POSIXct(time,tz='UTC',origin='1970-01-01'))
  return(aship)
  
}

# 读取原始文件路径 文件列表
filepaths=dir('~/AIS/2019_year_container/',full.names = TRUE)
# 提取文件名中的mmsi
mmsis=data.table(dir('~/AIS/2019_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi 
# 删除字符串开头和结尾的空格
mmsis=str_trim(mmsis)
# shipstopfiles:将路径和mmsi组成名为shipstopfiles0的数据表 
shipstopfiles=data.table(filepath=filepaths,mmsi=mmsis)
# shipn：文件行数
shipn=nrow(shipstopfiles)
# 读取shipstopfiles中第 个文件 
ship1=fread(shipstopfiles[99]$filepath)
# 添加 第小时年
ship1=addDateTime(ship1)
ship1=ship1[order(ship1$datetime),]
ship1=distinct(ship1)
# 对时间、经纬度和速度处理
ship1=data.table(addDateTime(ship1))[,list(mmsi,time,status,lon=lon/1000000,lat=lat/1000000,sod=speed,cog,head,datetime)]
# 添加 第小时/日 第日/月 第周/年 第月/年
ship1=ship1[,hour:=lubridate::hour(datetime)][,day:=lubridate::day(datetime)][,week:=lubridate::week(datetime)][,month:=lubridate::month(datetime)][,year:=lubridate::year(datetime)][,yday:=yday(datetime)]
# 去除 纬度异常值 去除对地航速异常值
ship1=ship1[lon<=180][sod>=0][,lon:=as.numeric(lon)][,lat:=as.numeric(lat)]
# 添加 第小时年
ship1=ship1[,yhour:=hour+(yday-1)*24]
# 计算两个点之间的时间差（单位s）dur是一个以秒为单位的船舶持续时间的向量
ship1=ship1[,dur:=shift(datetime,-1)-datetime][,dur:=as.integer(dur)]
# 添加 lat2 lon2 是下一个点的经纬度 speedid是船舶的速度等级的ID
ship1=ship1[,lon2:=shift(lon,-1)][,lat2:=shift(lat,-1)][!is.na(lon2)][,speedid:=round(as.numeric(sod))]#按航速分组
# 取得两个点之间的距离  dist是一个以海里为单位的船舶行驶距离的向量
ship1=ship1[,dist:=round(distance(lon,lat,lon2,lat2))]
# 统计不同时间段内不同航速的航行时间和航行距离
ship0=ship1[,list(.N,hours=round(sum(dur)/3600,2),distnm=round(sum(dist)/1852,2)),list(mmsi,speed=speedid,hour,yhour,day,week,month,year)]
# 加和不同速度下持续运行时间
#
betaSUM = ship1 %>%
     group_by(speedid) %>%
     summarise(dur=sum(dur))
