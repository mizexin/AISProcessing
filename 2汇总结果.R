library(data.table)
library(doParallel)
library(dplyr)
library(doSNOW)
library(tcltk)
# 输入年份
AIS_year ='2020'

dir = paste("/Volumes/Samsung\ SSD/AIS/",AIS_year,"EM",sep = '')
file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)
file_num <- length(file_list)
cl=makeCluster(10)
registerDoSNOW(cl)
# # bar =============================
# n <- file_num
# 
# #Progress combine function
# f <- function(){
#   pb <- txtProgressBar(min=1, max=n-1,style=3)
#   count <- 0
#   function(...) {
#     count <<- count + length(list(...)) - 1
#     setTxtProgressBar(pb,count)
#     Sys.sleep(0.01)
#     flush.console()
#     c(...)
#   }
# }
# bar =============================
pb <- tkProgressBar(max=file_num)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

#sum by ship=============================
x<-foreach(i=1:file_num,.packages=c('data.table'),.options.snow=opts,.combine=rbind) %dopar% {
  aresult=fread(paste(file_list[i],sep = ','),
             select =c("mmsi","imo","deadweight","capacity","distnm","ME_FC","AE_FC","AB_FC","ME_CO2","AE_CO2","AB_CO2","ME_SOx","AE_SOx","AB_SOx","ME_BC","AE_BC","AB_BC","ME_NOx","AE_NOx","AB_NOx","ME_CH4","AE_CH4","AB_CH4","ME_CO","AE_CO","AB_CO","ME_N2O","AE_N2O","AB_N2O","ME_PM10","AE_PM10","AB_PM10","ME_PM2.5","AE_PM2.5","AB_PM2.5","ME_NMVOC","AE_NMVOC","AB_NMVOC","gap_hours"))
  print(i)
  dt = as.data.table(lapply(aresult[,5:39],sum,na.rm=TRUE))
  dt=dt[,mmsi:=aresult[1,1]][,imo:=aresult[1,2]][,deadweight:=aresult[1,3]][,capacity:=aresult[1,4]]
  dt=dt[,Total_FC:=sum(dt$ME_FC,dt$AE_FC,dt$AB_FC)]
  dt=dt[,Total_CO2:=sum(dt$ME_CO2,dt$AE_CO2,dt$AB_CO2)]
  dt=dt[,Total_SOx:=sum(dt$ME_SOx,dt$AE_SOx,dt$AB_SOx)]
  dt=dt[,Total_BC:=sum(dt$ME_BC,dt$AE_BC,dt$AB_BC)]
  dt=dt[,Total_NOx:=sum(dt$ME_NOx,dt$AE_NOx,dt$AB_NOx)]
  dt=dt[,Total_CH4:=sum(dt$ME_CH4,dt$AE_CH4,dt$AB_CH4)]
  dt=dt[,Total_N2O:=sum(dt$ME_N2O,dt$AE_N2O,dt$AB_N2O)]
  dt=dt[,Total_PM10:=sum(dt$ME_PM10,dt$AE_PM10,dt$AB_PM10)]
  dt=dt[,Total_PM2.5:=sum(dt$ME_PM2.5,dt$AE_PM2.5,dt$AB_PM2.5)]
  dt=dt[,Total_NMVOC:=sum(dt$ME_NMVOC,dt$AE_NMVOC,dt$AB_NMVOC)]
  dt=dt[,No:=i]
  # fwrite(rbind(dt),paste('/Users/mizexin/论文数据/',AIS_year,'.csv',sep = ''),sep = ',',append = T)
  fwrite(rbind(dt),paste('/Volumes/Samsung\ SSD/汇总结果/fix/',AIS_year,'.csv'),sep = ',',append = T)
}
stopCluster(cl)

#sum by day=============================
x<-foreach(i=1:file_num,.packages=c('data.table','dplyr'),.options.snow=opts,.combine=rbind) %dopar% {
  aresult=fread(paste(file_list[i],sep = ','),
                select =c("mmsi","imo","deadweight","capacity","yday","distnm","gap_hours","ME_FC","AE_FC","AB_FC","ME_CO2","AE_CO2","AB_CO2","ME_SOx","AE_SOx","AB_SOx","ME_BC","AE_BC","AB_BC","ME_NOx","AE_NOx","AB_NOx","ME_CH4","AE_CH4","AB_CH4","ME_CO","AE_CO","AB_CO","ME_N2O","AE_N2O","AB_N2O","ME_PM10","AE_PM10","AB_PM10","ME_PM2.5","AE_PM2.5","AB_PM2.5","ME_NMVOC","AE_NMVOC","AB_NMVOC"))
  
  if(nrow(aresult) != 0)
  {
    dt = as.data.table(aggregate(aresult[,6:40],by=list(aresult$yday),sum)%>%
                         rename(.,'yday'= 'Group.1'))
    dt = dt[,FC:=ME_FC+AE_FC+AB_FC][,CO2:=ME_CO2+AE_CO2+AB_CO2][,SOx:=ME_SOx+AE_SOx+AB_SOx][,BC:=ME_BC+AE_BC+AB_BC][,NOx:=ME_NOx+AE_NOx+AB_NOx][,CH4:=ME_CH4+AE_CH4+AB_CH4][,N2O:=ME_N2O+AE_N2O+AB_N2O][,PM10:=ME_PM10+AE_PM10+AB_PM10][,PM2.5:=ME_PM2.5+AE_PM2.5+AB_PM2.5][,NMVOC:=ME_NMVOC+AE_NMVOC+AB_NMVOC]
    dt0 = dt[,.(yday,distnm,gap_hours,FC,CO2,SOx,BC,NOx,CH4,N2O,PM10,PM2.5,NMVOC)]
    
    fwrite(rbind(dt0),paste('/Volumes/Samsung\ SSD/汇总结果/fix/',AIS_year,'yday.csv'),sep = ',',append = T)
}  }
stopCluster(cl)


