library(data.table)
library(doParallel)
library(dplyr)
library(doSNOW)
library(tcltk)
library(ggplot2)
# 输入年份
AIS_year ='2020'

# dir = paste("~/AIS/",AIS_year,"-stop-result",sep = '')
dir = paste("/Volumes/Samsung\ SSD/AIS/",AIS_year,"EM",sep = '')

file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)
file_num <- length(file_list)
cl=makeCluster(10)
registerDoSNOW(cl)
# bar =============================
pb <- tkProgressBar(max=file_num)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

# select abnormal files in all=============================
x<-foreach(i=1:file_num,.packages=c('data.table','dplyr'),.options.snow=opts,.combine=rbind) %dopar% {
  aresult=fread(paste(file_list[i],sep = ''),
                select =c("mmsi","imo","deadweight","capacity","yhour","yday","distnm","gap_hours","ME_FC","AE_FC","AB_FC"))
  mmsis=aresult[,.N,mmsi]$mmsi
  k=length(mmsis)
  if(nrow(aresult) == 1)
     next
  for (j in seq(1,k))
  {
    ammsi=mmsis[j] #单跑改k
    # abnormal days 
    dt = aresult[yday == 30  | 
                   yday == 31  |
                   yday == 91  | 
                   yday == 126 | 
                   yday == 127 | 
                   yday == 128 ]
    dt=dt[,FC:=ME_FC+AE_FC+AB_FC]
    dt = as.data.table(aggregate(dt[,7:12],by=list(dt$mmsi,dt$yday),sum)%>%
                         rename(.,'mmsi'= 'Group.1','yday'= 'Group.2'))
    fwrite(rbind(dt),paste('//Volumes/Samsung\ SSD/abnormal/temp.csv',sep = ''),sep = ',',append = T)
  }
}
stopCluster(cl)

temp2019=fread('/Volumes/Samsung\ SSD/abnormal/temp2019.csv')

day30 = temp2019[yday == 30]
day31 = temp2019[yday == 31]
day30 %>%
  ggplot( aes(x = mmsi, y = FC/1000000)) +
  geom_point() +
  ggtitle("") +
  # theme_ipsum() +
  ylab("FC") +
  # geom_label( x=1990, y=55000, label="Amanda reached 3550\nbabies in 1970", size=4, color="#69b3a2") +
  theme( )

# select abnormal files=============================
x<-foreach(i=1:file_num,.packages=c('data.table','dplyr'),.options.snow=opts,.combine=rbind) %dopar% {
  aresult=fread(paste(file_list[i],sep = ''),
                select =c("mmsi","imo","deadweight","capacity","yhour","yday","distnm","gap_hours","ME_FC","AE_FC","AB_FC","ME_CO2","AE_CO2","AB_CO2","ME_SOx","AE_SOx","AB_SOx","ME_BC","AE_BC","AB_BC","ME_NOx","AE_NOx","AB_NOx","ME_CH4","AE_CH4","AB_CH4","ME_CO","AE_CO","AB_CO","ME_N2O","AE_N2O","AB_N2O","ME_PM10","AE_PM10","AB_PM10","ME_PM2.5","AE_PM2.5","AB_PM2.5","ME_NMVOC","AE_NMVOC","AB_NMVOC"))
  mmsis=aresult[,.N,mmsi]$mmsi
  k=length(mmsis)
  # if(nrow(aresult) == 0)
  #   next
  for (j in seq(1,k))
  {
    ammsi=mmsis[j] #单跑改k
    # abnormal days 
    dt = aresult[yday == 30  | 
                   yday == 31  |
                   yday == 91  | 
                   yday == 126 | 
                   yday == 127 | 
                   yday == 128 ]
    fwrite(rbind(dt),paste('/Volumes/Samsung\ SSD/abnormal/',AIS_year,'/',ammsi,'.csv',sep = ''),sep = ',',append = T)
  }
}
stopCluster(cl)