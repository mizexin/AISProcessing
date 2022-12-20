#new function in 2020
library(dplyr)
library(data.table)
library(mapview)
library(sf)
library(lubridate)
library(ggplot2)

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
#sf_tracks=st_as_sf(tracks,coords = c('lon','lat'))%>%st_set_crs(4326)
#sf_trips <- sf_tracks %>%group_by(mmsi,tripid) %>%summarise(do_union = FALSE) %>%st_cast("LINESTRING")
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
#system.time(dm1<-sf_dtwdistance(sf_trips))
#system.time(dm2<-dtwDistance(sl))#this method a little bit faster

#https://github.com/Kersauson/ST-DBSCAN/blob/master/stdbscan.R
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
  
# eca=fread('D:/Git/Rprojects/WBC/data_in/china_eca_polygon.csv');
# setkey(eca,id)
# ships=fread('D:/Git/Rprojects/WBC/data_in/wbc_4ship_2018.csv')
# aship=ships[mmsi==356289000];setkey(aship,mmsi,time)
# aship_eca=getPointInECA(aship,eca)
# aship_eca[,.N,eca]

#detect stops from a ship
# ships=fread('D:/Git/Rprojects/WBC/data_in/wbc_4ship_2018.csv')
# aship=ships[mmsi==356289000]
# setkey(aship,mmsi,time)
# aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon,lat,sog,cog,head,datetime)]

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





