#data2individual ship trajectory
library(data.table)
library('foreach')
library('doParallel')

Sys.setlocale("LC_ALL", 'English')
ships_container=fread("D:/share/2020AIS/hifleetships_brief_container.csv")[!is.na(mmsi)][mmsi>100000000]
mmsis_container=ships_container[,.N,list(mmsi)]$mmsi
ships_commercial=fread("D:/share/2020AIS/hifleetships_brief_commercial.csv")[!is.na(mmsi)][mmsi>100000000]
mmsis_commercial=ships_commercial[,.N,list(mmsi)]$mmsi
dt_month_container=data.table(mmsi=0,time=0,status=0,lon=0,lat=0,speed=0,cog=0,head=0)[mmsi<0]
#

cl=makeCluster(8)#利用8个处理器
registerDoParallel(cl)
getDoParWorkers()

x<-foreach(j=20200301:20200331,.packages=c('data.table')) %dopar% { 
  #j=20200101
  fwrite(data.table(x=0),paste('D:/share/AIS/multicore_log/',j,'.csv'))
  filepath=paste("D:/share/2020AIS/compress_202003/compress",j,".GTMS3",sep = '')
  dt=fread(filepath)
  setnames(dt,c('mmsi','time','type','supplier','source','status','lon','lat','areaid','speed','conversion','cog','head','power','imitator','extend'))
  dt=dt[,list(mmsi,time,status,lon,lat,speed,cog,head)]
  dt=dt[mmsi%in%mmsis_container]
}

x<-foreach(file_list,.packages=c('data.table'),.combine='rbind') %dopar% { 
  #i=10
  print(i)
  agroup=fread(paste('~/AIS/2019_year_container/',i,'.csv',sep = ''),
               col.names = c('mmsi','time','status','lon','lat','sog','cog','head'), showProgress = T, nThread = 12)
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
    fwrite(aship_stops,paste('~/AIS/2019-stop-result/',ammsi,'.csv',sep = ''))
  }
}

stopCluster(cl)



(ii=1:100,.combine = "c",.export = c("semimetric.pca","quadratic"))%dopar% func(ii)

setPoints <- function(points,scale = 10) {
  n = nrow(points)
  
  points[,pid:= seq(1,n,1)]
  points[,g.lon:= floor(lon * scale) / scale]
  points[,g.lat:= floor(lat * scale) / scale]
  points[,gid:= paste(floor(lon * scale) / scale,floor(lat * scale) / scale,sep =
                        '_')]
  
  #setkey(points,time)
  
  return(points)
  
}
