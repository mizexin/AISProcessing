library(data.table)
library(dplyr)
library(lubridate)
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

aship=fread(paste("/Volumes/Samsung\ SSD/sp_time_test.csv",sep = ''),
            col.names = c('mmsi','time','status','lon','lat','speed','cog','head'))

aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon=lon/1000000,lat=lat/1000000,sod=speed,cog,head,datetime)]

dateTimeObs = aship$datetime;dateTimeObs
dateTimeAll = seq(min(dateTimeObs)
                  ,max(dateTimeObs),24*60*60);dateTimeAll

#先把时间列不全的小数据框造出来,然后合并后用时间列进行排序

nROW<-length(dateTimeAll)-length(dateTimeObs)
nCOL<-ncol(aship)-1
partDateTime<-dateTimeAll[!dateTimeAll%in%dateTimeObs]

partData<-cbind.data.frame(matrix(rep (NA, nROW*nCOL), nROW, nCOL),floor_date(partDateTime, unit = "day"))

names(partData) <-names(aship)
aship<-rbind (aship, partData)

aship=aship[order(aship$datetime),]

aship$mmsi=aship$mmsi[1]

aship_approx_lat = approx(x = aship$datetime, y = aship$lat,xout = seq(min(aship$datetime), max(aship$datetime),"mins"))
aship_approx_lon = approx(x = aship$datetime, y = aship$lon,xout = seq(min(aship$datetime), max(aship$datetime),"mins"))
df = data.frame(aship_approx_lat[["x"]],aship_approx_lat[["y"]])
plot(aship_approx_lat$y,aship_approx_lon$y)

points(aship$lon, aship$lat,
       col = "red",
       pch = 16)


library(lubridate)
library(data.table)
library(dplyr)
library(mapdeck)
library(echarts4r)
#载入函数
source('/Users/mizexin/R/AIS_Processing/functions2020-copy.R')

#使船舶轨迹数据字段：mmsi,time,sog,lon,lat 五个字段，并按照time排序
ashipdt=fread('/Volumes/Samsung\ SSD/points_413623000.csv')[,mmsi:=413623000][,shipSpeed:=shipSpeed*10][,list(mmsi,time=timestamp,sog=shipSpeed,lon=longitude,lat=latitude)] |> setkey(time)
ashipdt=ashipdt[,datetime:=lubridate::as_datetime(time,tz='UTC')]

lines=getLines(ashipdt)
speedlines=getLineSpeed(lines)
abnormallines=getAbnormalLine(speedlines,timelimit1 = 6,timelimit2 = 12) # 通过调整这个参数了是被异常线段，超过limit1且平均航速小于3节，或者 大雨limit2 都为异常。另一个条件是航速超过37.5也为异常
abnormallines[isabnormal>0]
#提取异常线段中的开始和结束时间
ablines=abnormallines[isabnormal>0][,list(datetime1,datetime2)] #提取异常线段两个端点的时间
#天插值
ashipdt2=add_day_point(ashipdt[,list(mmsi,time=time1,sog=sog1,lon=lon1,lat=lat1,datetime=datetime1)])
#去除异常线段中的插值，之后就可以按照原来的程序计算排放了
ashipdt3=ashipdt2[!(isaddedday>0&inrange(datetime,ablines$datetime1,ablines$datetime2))]

#画图
ashipdt3 |>
  group_by(isaddedday) |>
  e_chart(datetime)|> 
  e_line(sog) |>
  e_datazoom(type = "slider") 


library(doSNOW)
cl <- makeCluster(2)
registerDoSNOW(cl)
iterations <- 100
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .combine = rbind, 
                  .options.snow = opts) %dopar%
  {
    s <- summary(rnorm(1e6))[3]
    return(s)
  }
close(pb)
stopCluster(cl) 