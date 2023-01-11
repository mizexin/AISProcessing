#new function in 2020
library(dplyr)
library(data.table)
library(mapview)
library(sf)
library(lubridate)
library(ggplot2)
library(circular)

addDateTime<-function(aship){#aship has timestamp column as time, timezone is set to UTC,use lubridate package
  
  aship=mutate(aship,datetime=as.POSIXct(time,tz='UTC',origin='1970-01-01'))
  return(aship)
  
}

addUTMXY<-function(aship){ #load dpylr,sf,data.table first, aship must have lon,lat columns
  setkey(aship,mmsi,time)
  aship=data.table(aship)[,geomlon:=lon][,geomlat:=lat]
  aship=aship[,utmzone:=(floor((lon + 180) / 6) %% 60) + 1][,utm:=utmzone+32700][lat>0,utm:=utmzone+32600]
  utmzones=aship[,.N,list(utm)]
  aship_sf_points=st_as_sf(aship,coords = c('geomlon','geomlat'))%>%st_set_crs(4326)
  aship_sf_points_utm=mutate(aship_sf_points,X=0,Y=0)%>%filter(mmsi<0);aship_sf_points_utm$geometry=NULL
  for(i in seq(1,nrow(utmzones))){
    print(i)#i=1
    zone=utmzones[i]$utm
    dt=filter(aship_sf_points,utm==zone)
    temp<-st_transform(dt,zone);xy=st_coordinates(temp)
    temp=cbind(data.table(dt)[,geometry:=NULL],xy)
    aship_sf_points_utm=rbind(aship_sf_points_utm,temp)
  }
  aship_sf_points_utm$utmzone=NULL
  return(aship_sf_points_utm)
}

addGrid<-function(aship,scale){return(data.table(aship)[,glon:=floor(lon*scale)/scale][,glat:=floor(lat*scale)/scale])}

addPointGeom<-function(aship){
  aship=data.table(aship)[,lon0:=lon][,lat0:=lat];
  aship_sf_points=st_as_sf(aship,coords = c('lon0','lat0'))%>%st_set_crs(4326);
  return(aship_sf_points)
}

toLines<-function(aship){
  nr=nrow(aship)
  test=aship%>%mutate(lon1=shift(lon,n=-1))%>%mutate(lat1=shift(lat,n=-1))%>%mutate(time1=shift(time,n=-1))%>%
    mutate(head1=shift(head,n=-1))%>%mutate(sog1=shift(sog,n=-1))%>%mutate(cog1=shift(cog,n=-1))
  return(test)
}

addLineGeom<-function(aship){
  nr=nrow(aship)
  test=aship%>%mutate(lon1=shift(lon,n=-1))%>%mutate(lat1=shift(lat,n=-1))%>%mutate(time1=shift(time,n=-1))%>%
    mutate(head1=shift(head,n=-1))%>%mutate(sog1=shift(sog,n=-1))%>%mutate(cog1=shift(cog,n=-1))
  l=mutate(test[1:(nr-1),],l=st_as_sfc(paste('LINESTRING (',lon,' ',lat,',',lon1,' ',lat1,')',sep='')))#%>%st_sfc()
  lsf=st_sf(l)%>%st_set_crs(4326)
  return(lsf)
}

sf_dtwdistance<-function(sf_trips){
  n=nrow(sf_trips); 
  distmat <- matrix(rep(0, n * n), nrow = n)# prepare empty distance matrix
  # fill the matrix
  for (i in 1:(n - 1)) {
    if(i%%10==0){print(i)}
    trip1= sf_trips[i,]%>%st_coordinates() %>% st_linestring()
    for (j in (i + 1):n) {
      trip2= sf_trips[j,]%>%st_coordinates() %>% st_linestring()
      align <- dtw(trip1, trip2)
      distmat[i, j] <- distmat[j, i] <- align$normalizedDistance  # normalized distance
      
    }
  }
  return(distmat)
}

# function to calculate dwt distance between spatial lines
dtwDistance <- function(spgeom1, spgeom2 = NULL) {
  # if second set of lines is not given, calculate pairwise distances within
  # first set of lines
  if (is.null(spgeom2)) {
    
    # prepare empty distance matrix
    n_geoms <- length(spgeom1)
    distmat <- matrix(rep(0, n_geoms^2), ncol = n_geoms)
    
    # fill the matrix
    for (i in 1:(n_geoms - 1)) {
      if(i%%10==0){print(i)}
      crds1 <- coordinates(spgeom1[i, ])[[1]][[1]]
      for (j in (i + 1):n_geoms) {
        
        crds2 <- coordinates(spgeom1[j, ])[[1]][[1]]
        align <- dtw(crds1, crds2)
        distmat[i, j] <- distmat[j, i] <- align$normalizedDistance  # normalized distance
        
      }
    }
    
    # if two sets of lines are given, calculate pairwise distances
  } else {
    
    # prepare empty distance matrix
    n_geoms1 <- length(spgeom1)
    n_geoms2 <- length(spgeom2)
    distmat <- matrix(rep(0, n_geoms1 * n_geoms2), nrow = n_geoms1)
    
    # fill the matrix
    for (i in 1:n_geoms1) {
      if(i%%10==0){print(i)}
      crds1 <- coordinates(spgeom1[i, ])[[1]][[1]]
      for (j in 1:n_geoms2) {
        
        crds2 <- coordinates(spgeom2[j, ])[[1]][[1]]
        align <- dtw(crds1, crds2)
        distmat[i, j] <- align$normalizedDistance
        
      }
    }
    
  }
  return(distmat)
}

stdbscan = function (traj,x,y,time,eps,eps2,minpts,cldensity = TRUE) { 
  
  countmode = 1:length(x)
  seeds = TRUE
  
  data_spatial<- as.matrix(dist(cbind(y, x)))
  data_temporal<- as.matrix(dist(time))
  n <- nrow(data_spatial)
  
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)
  
  for (i in 1:n) {
    if (i %in% countmode)
      #cat("Processing point ", i, " of ", n, ".\n")
      unclass <- (1:n)[cv < 1]
    
    if (cv[i] == 0) {
      reachables <- intersect(unclass[data_spatial[i, unclass] <= eps],  unclass[data_temporal[i, unclass] <= eps2])
      if (length(reachables) + classn[i] < minpts)
        cv[i] <- (-1)                    
      else {
        cn <- cn + 1                   
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)       
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          cv[reachables] <- cn           
          ap <- reachables                           
          reachables <- integer()
          
          for (i2 in seq(along = ap)) {
            j <- ap[i2]
            
            jreachables <- intersect(unclass[data_spatial[j, unclass] <= eps], unclass[data_temporal[j, unclass] <= eps2])
            
            if (length(jreachables) + classn[j] >= minpts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
    
  }
  
  
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  out <- list(cluster = cv, eps = eps, minpts = minpts, density = classn)
  rm(classn)
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "stdbscan"
  return(out)
}

#stdbscan only support limited number of points for speed, need to group
groupP0<-function(p0,npoint=10000,nhour=5){
  pids=p0[,pid1:=shift(pid,n=-1)][,time1:=shift(time,n=-1)][,dur:=time1-time][dur>nhour*3600]$pid
  segn=(nrow(p0)/npoint)%>%floor();
  if(segn>0){
    pids1=pids[sapply(seq(npoint,segn*npoint,npoint), FUN = function(x) {which.min(abs(pids-x))})];pids2=c(1,pids1,nrow(p0))
    for(i in seq(1,(length(pids2)-1))) { 
      if (pids2[i]!=pids2[i+1]){
        p0=p0[pids2[i]:pids2[i+1],group:=i]
      }
    }
    return(p0)
  }else{
    p0=p0[,group:=1]
    return(p0)
  }
  
}

#get the points of ship in emission contral area
getPointInECA<-function(aship,eca){
  aship_sf=addPointGeom(aship)
  line_sfc=st_linestring(x=as.matrix(eca[,list(lon,lat)]))%>%st_sfc()%>%st_set_crs(4326)
  #line_sf=st_sf(area='chinaeca',geom=line_sfc)
  poly_sfc=st_polygon(line_sfc)%>%st_sfc(crs=4326)
  #mapview(poly_sfc)
  res=st_intersects(poly_sfc,aship_sf)
  return(aship[,eca:=0][res[[1]],eca:=1])
  #mapview(poly_sfc)+sample_n(aship_sf[res[[1]],],10000)%>%mapview()
}

detectShipStops<-function(aship,cutspeed=10,groupnpoints=8000,groupgaphours=5,dbscanspatialeps=0.005,dbscantimeesp=7200,dbscanminpts=2){
  setkey(aship,mmsi,time)
  aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon,lat,sog,cog,head,datetime)]
  #aship_sf_points=st_as_sf(aship[,lon0:=lon][,lat0:=lat],coords = c('lon0','lat0'))%>%st_set_crs(4326)
  p0=aship[sog<cutspeed];p0=p0[,pid:=seq(1:nrow(p0))]
  #stdbscan can only deal with limited number of point depend on pc, ususlly not > 10000
  p0=groupP0(p0,groupnpoints,groupgaphours);groups=p0[,.N,group];groupn=nrow(groups);groups
  stops=data.table(p0,cl='')[mmsi<0]
  for (i in seq(1:groupn)){
    print(i)#i=11
    agroup=groups[i]$group
    test=p0[group==agroup]
    
    nr=nrow(test)
    if(nr<=groupnpoints){
      res=stdbscan(test,test$lon,test$lat,test$time,dbscanspatialeps,dbscantimeesp,dbscanminpts)
      clusters=data.table(cbind(test,cl=as.character(res$cluster)))[cl>0][,cl:=paste(group,cl,sep = '_')]
      stops=rbind(stops,clusters)
      
    }else{
      
      subgroups=split(test, (seq(nrow(test))-1) %/% groupnpoints) 
      ng=length(subgroups)
      for (gi in seq(1,ng)){
        #gi=1
        asubgroup=data.table(subgroups[[gi]])
        res=stdbscan(asubgroup,asubgroup$lon,asubgroup$lat,asubgroup$time,dbscanspatialeps,dbscantimeesp,dbscanminpts)
        clusters=data.table(cbind(asubgroup,cl=as.character(res$cluster)))[cl>0][,cl:=paste(group,gi,cl,sep = '_')]#add subgroup id to cluster id 
        stops=rbind(stops,clusters)
      }
      
    }
    
  }
  #stops[,.N,cl]
  setkey(stops,mmsi,time)
  groupStartEnd=cbind(stops[,.SD[1,list(mmsi,time1=time)],cl][,list(mmsi,time1)],stops[,.SD[.N,list(time2=time)],cl][,list(time2,cl)])
  aship=aship[,cl:='0']
  for (i in (1:nrow(groupStartEnd))){
    if(i%%10==0){print(i)}
    agroup=groupStartEnd[i]
    aship[time%between%c(agroup$time1,agroup$time2),cl:=agroup$cl]
  }
  return(aship)
}

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

setLines <- function(points) {
  n = nrow(points)
  setkey(points,time)
  dt1 = points[1:(n - 1),list(mmsi,lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  dt2 = points[2:n,list(lon,lat,time,sog,pid,gid,g.lon,g.lat)]
  lines = cbind(dt1,dt2)
  setnames(
    lines,c('mmsi','lon1','lat1','time1','sog1','pid1','gid1','g.lon1','g.lat1','lon2','lat2','time2','sog2','pid2','gid2','g.lon2','g.lat2'
    )
  )
  #lines[,lid:= seq(1,n - 1)] #原始代码
  lines[,lid:=as.character( seq(1,n - 1))] #为了phd 长三角排放控制区代码
  
  return(lines)
  
}

#lines:data.table
#speed unit:knot(nm/h)
addLineSpeed <- function(lines,time_threshold = 600,dist_threshold = 2) {
  lines = lines[,timespan:= abs(time2 - time1) * 1.0]
  lines = lines[,distance:= distance(lon1,lat1,lon2,lat2)]
  lines[,avgspeed1:= round((sog1 + sog2) / 2,1)]
  lines[,avgspeed2:= round((distance / 1852) * 10 / (timespan / 3600))]# lavgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:= avgspeed1];
  lines[(distance / 1852 > dist_threshold) |
          (timespan > time_threshold),avgspeed:= avgspeed2]
  return(lines)
  
}
#区别

#distance of two points,单位米
distance <- function(lon1,lat1,lon2,lat2) {
  radlat1 = rad(lat1);
  radlat2 = rad(lat2);
  delta_lon = rad(lon2 - lon1);
  top1 = cos(radlat2) * sin(delta_lon);
  top2 = cos(radlat1) * sin(radlat2) - sin(radlat1) * cos(radlat2) * cos(delta_lon);
  top = sqrt(top1 * top1 + top2 * top2);
  bottom = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(delta_lon);
  delta_sigma = atan2(top,bottom);
  distance = delta_sigma * 6378137.0;
  return (distance);
  
}

#首先需运行以下程序，使这些函数生效
#points: mmsi,time,sog,lon,lat,datetime

getLines <- function(dt) { # a ship dt 
  setkey(dt,time)
  setnames(dt,c('mmsi','time1','sog1','lon1','lat1','datetime1'))
  dt=dt[,time2:=shift(time1,-1)]
  dt=dt[,sog2:=shift(sog1,-1)]
  dt=dt[,lon2:=shift(lon1,-1)]
  dt=dt[,lat2:=shift(lat1,-1)]
  dt=dt[,datetime2:=shift(datetime1,-1)]
  dt=dt[!is.na(time2)][,lid:= seq(1,(nrow(dt) - 1))]
  return(dt)
}

#lines:data.table
#speed unit:knot(nm/h)

getLineSpeed <- function(lines,time_threshold=4,dist_threshold = 10) { # 距离超过10海里或则时间超过4个小时，都采用航行距离除以航行时间（小时）
  lines = lines[,durhour:= round(abs(time2 - time1) * 1.0/3600,2)]
  lines = lines[,distnm:= round(distance(lon1,lat1,lon2,lat2)/1852,1)]
  lines[,inst_avgspeed1:= round((sog1 + sog2) / 2)]
  lines[,dist_avgspeed2:= round(distnm * 10 / (durhour))]# avgspeed 与 sog 单位相同 海里/小时*10
  lines[,avgspeed:= inst_avgspeed1];
  lines[(distnm > dist_threshold) | (durhour > time_threshold),avgspeed:= dist_avgspeed2]
  
  return(lines)
  
}

#计算弧度
rad <- function(d) {
  return (d * pi / 180);
}

getAbnormalLine<-function(lines,timelimit1=72,timelimit2=240,speedlimit=37.5){ #小时，节
  lines[,isabnormal:=0]
  lines[(durhour>=timelimit1&avgspeed<3)|durhour>=timelimit2|avgspeed>=speedlimit*10,isabnormal:=1] #时间间隔大于72小时，同时平均航速小于3,这里的平均航速与sog相同单位
}

add_day_point<-function(aship){
  
  minday=min(as.Date(aship$datetime))+1#最小值加上一天
  maxday=max(as.Date(aship$datetime))
  dt_speed=approx(x=aship$datetime,y=aship$sog, xout=seq(as_datetime(minday),as_datetime(maxday), "days"))#转化为datetime,speed插值
  dt_lon=approx(x=aship$datetime,y=aship$lon, xout=seq(as_datetime(minday),as_datetime(maxday), "days"))#lon 插值
  dt_lat=approx(x=aship$datetime,y=aship$lat, xout=seq(as_datetime(minday),as_datetime(maxday), "days"))#lat 插值
  dt=data.table(mmsi=aship[1]$mmsi,time=as.numeric(as.POSIXct(dt_speed$x, format="%Y-%m-%d")),sog=round(dt_speed$y,1),lon=round(dt_lon$y,6),lat=round(dt_lat$y,6),datetime=dt_speed$x)
  dt=dt[,isaddedday:=1]
  aship2=rbind(aship[,isaddedday:=0],dt) |> setkey(time)
  return(aship2)
  
}

AddInfo <- function(aship,time_threshold=4,dist_threshold = 10) { 
  setkey(aship,time)
  setnames(aship,c('mmsi','time1','sog1','lon1','lat1','datetime1','isaddedday'))
  aship[,time2:=shift(time1,-1)]
  aship[,sog2:=shift(sog1,-1)]
  aship[,lon2:=shift(lon1,-1)]
  aship[,lat2:=shift(lat1,-1)]
  aship[,datetime2:=shift(datetime1,-1)]
  aship[!is.na(time2)][,lid:= seq(1,(nrow(aship) - 1))]
  aship[,durhour:= round(abs(time2 - time1) * 1.0/3600,2)]
  aship[,distnm:= round(distance(lon1,lat1,lon2,lat2)/1852,1)]
  aship[,inst_avgspeed1:= round((sog1 + sog2) / 2)]
  aship[,dist_avgspeed2:= round(distnm * 10 / (durhour))]
  aship[,avgspeed:= inst_avgspeed1/10]; #节
  aship[(distnm > dist_threshold) | (durhour > time_threshold),avgspeed:= dist_avgspeed2/10] #节
  aship=data.table(mmsi=aship[1]$mmsi,time=aship$time1,sog=aship$sog1,lon=aship$lon1,lat=aship$lat1,datetime=aship$datetime1,durhour=aship$durhour,distnm=aship$distnm,avgspeed=aship$avgspeed,isaddedday=aship$isaddedday)
  return(aship)
}

SplitTimeFormat <- function(aship){
  aship[,hour:=lubridate::hour(datetime)][,day:=lubridate::day(datetime)][,week:=lubridate::week(datetime)][,month:=lubridate::month(datetime)][,yyear:=lubridate::year(datetime)][,yday:=yday(datetime)][,yhour:=hour+(yday-1)*24]
}

