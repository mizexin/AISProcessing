#realship validation

library(RODBC)
library(foreach)
library(data.table)
library(lubridate)
library('dplyr')
library('stringr')
library('ggplot2')
library('ggthemes')
library(ggpubr)

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

rad<-function(d){  
  return (d * pi/180);   
}

# #---------------read from mdb------------
# con <- odbcConnectAccess('D:/Git/Rprojects/ningbo/ningbo/data/realship/badayanData202012.mdb')
# ship1=foreach(i = seq(1201,1231),.combine=rbind) %do% {
#   tbname=paste('DataTable2020',i,sep = '')
#   print(tbname)
#   temp=sqlFetch(con, tbname)
# }
# odbcClose(con)
# ship1=data.table(ship1)
# ship1=ship1[, c("lat", "lat_D") := tstrsplit(Latitude, " ", fixed=TRUE)]
# ship1=ship1[, c("lon", "lon_D") := tstrsplit(Longitude, " ", fixed=TRUE)]
# ship1=ship1[,datetime:=lubridate::as_datetime(paste(PCDate,PCTime))]
# 
# fwrite(ship1,'D:/Git/Rprojects/ningbo/ningbo/data/realship/shuwuData202012.csv')
# fwrite(ship1,'D:/Git/Rprojects/ningbo/ningbo/data/realship/badayanData202012.csv')
# 
# ship1=fread('D:/Git/Rprojects/ningbo/ningbo/data/realship/shuwuData202012.csv')
ship1=fread('/Users/mizexin/AIS/badayanData202012.csv')
ship1=ship1[,mmsi:='477401900'][,imo:='9845738']
setkey(ship1,mmsi,datetime)
ship1=ship1[Longitude<=180][ShipSpd>=0][,lon:=as.numeric(lon)][,lat:=as.numeric(lat)]
ship1=ship1[,hour:=lubridate::hour(datetime)][,day:=lubridate::day(datetime)][,week:=lubridate::week(datetime)][,month:=lubridate::month(datetime)][,year:=lubridate::year(datetime)][,yday:=yday(datetime)]
ship1=ship1[,yhour:=hour+(yday-1)*24]
ship1=ship1[,dur:=shift(datetime,-1)-datetime][,dur:=as.integer(dur)]#计算两个点之间的距离
ship1=ship1[,lon2:=shift(lon,-1)][,lat2:=shift(lat,-1)][!is.na(lon2)][,speedid:=round(as.numeric(ShipSpd)*10)]#按航速分组
ship1=ship1[,dist:=round(distance(lon,lat,lon2,lat2))][,SHIP_SPEED:=19][,SHIP_ENGINE_POWER:=13700]
ship1=ship1[,loadFactor:=round(MEShaftPow/SHIP_ENGINE_POWER,2)]
ship1=ship1[,ME_FC_kwh:=(MESFOC_kw*(0.445*loadFactor^2-0.710*loadFactor+1.280)*MEShaftPow*dur/3600)/1000/1000][,ME_FC_nm:=(MESFOC_nmile*dist/1852)/1000]#MESFOC_kw这个如何计算得到的？
ship1=ship1[,ME_FC_kwh_cum:=cumsum(MESFOC_kw*(0.445*loadFactor^2-0.710*loadFactor+1.280)*MEShaftPow*dur/3600)/1000/1000][,ME_FC_nm_cum:=cumsum(MESFOC_nmile*dist/1852)/1000]
#ship_hour_speed=ship1[,list(.N,dur=sum(dur),dist=round(sum(dist))),list(mmsi,imo,yhour,speedid)] #dur单位秒，dist 米


ship0=ship1[,list(.N,hours=round(sum(dur)/3600,2),distnm=round(sum(dist)/1852,2)),list(mmsi,SHIP_IMO=imo,speed=speedid,hour,yhour,day,week,month,year)]#统计不同时间段内不同航速的航行时间和还行距离

ship0=ship0[,SHIP_TYPE_CODE:='集装箱船'][,SHIP_SPEED:=19][,TEUCapacity:=2433][,SHIP_Draught:=11.500][,SHIP_BREADTH:=32.287][,SHIP_DEPTH:=17.100][,SHIP_LENGTH:=188.778][,SHIP_DWT:=35337][,SHIP_GROSSTON:=26771]
ship0=ship0[,SHIP_BUILT_DATE:='2019-11-01'][,SHIP_ENGINE_POWER:=13700][,SHIP_BUILT_YEAR:=2019][,stroke:=2][,ME_engineType:='SSD'][,shipAndCargType:=70] #rpm=97 #补充相关船舶档案数据

#---注意：以下代码采用AIS数据，需要对相应字段进行调整后再分析----------------


full_ships0 = ship0

AE_AB_Power=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')

test=AE_AB_Power[,.N,list(Ship_Type=ShipType,SHIP_TYPE_CODE=ShipType_cn)][full_ships0,on=.(SHIP_TYPE_CODE),nomatch=NULL]

#需要针对不同船型，
# test_bulk=test[SHIP_TYPE_CODE=='散货船'&(SHIP_DWT>=Size_min)&(SHIP_DWT<=Size_max)]
# test_container=test[SHIP_TYPE_CODE=='集装箱船'&(TEUCapacity>=Size_min)&(TEUCapacity<=Size_max)]
# test=rbind(test_bulk,test_container)


# explore(test0,FL)
# explore_all(full_ships)

#----------重要假设----------------
#主机发动机类型：SSD
#发动机冲程：two_stroke
#主机燃油类型：DMO
#副机发动机：Auxiliary_Engines
#锅炉发动机：Steam_Boiler
#副机和锅炉也多是DMO
#需要增加副机和锅炉发动机类型
#增加发动机代次gen
#增加发动机的tier
#加一列capacity：集装箱用TEU数据

# test0=data.table(inner_join(ships[speed<300&SHIP_SPEED<300,list(mmsi,speed,hours,distnm,shipAndCargType)],test,by='mmsi'))
# test0=test0[!is.na(TEUCapacity)]
#test0=test0[,loadFactor:=((speed/10)/(SHIP_SPEED/0.94))^3][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
#这里的天气和外壳的调整系数需要进一步查表得到nw,nf p153 table44
test0=test[,loadFactor:=round(((speed/10)/(SHIP_SPEED/0.94))^3,2)/0.867/0.917][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
#test0[,N:=NULL];test0$vesselType=NULL;test0$lengthOverall=NULL;test0$i.N=NULL

test0=test0[,ME_engineType:='SSD'][,AE_engineType:='Auxiliary_Engines'][,AB_engineType:='Steam_Boiler']
test0=test0[,ME_fuelType:='HFO'][,AE_fuelType:='MDO'][,AB_fuelType:='MDO']
test0=test0[,gen:=fcase(SHIP_BUILT_YEAR <1984,1,
                        SHIP_BUILT_YEAR%between%c(1984,2000),2,
                        SHIP_BUILT_YEAR>=2001,3)]
test0=test0[,tier:=fcase(SHIP_BUILT_YEAR<=2000,0,
                         SHIP_BUILT_YEAR%between%c(2001,2010),1,
                         SHIP_BUILT_YEAR%between%c(2011,2015),2,
                         SHIP_BUILT_YEAR>=2016,3)]
test0=test0[,mode:=fcase(speed>=0&speed<10,'berth',
                         speed>=10&speed<30,'anchor',
                         speed>=30&loadFactor<=0.2,'manoeuvring',
                         speed>=30&loadFactor>=0.2&loadFactor<0.65,'sea',
                         loadFactor>=0.65,'sea')]

#注：这个Liquefied_gas_tanker船舶需要进一步用cbm单位，目前用gt代替。

test0=test0[,capacity:=SHIP_GROSSTON][,unit:='gt'][,capacity:=fcase(Ship_Type%in%c('Bulk_carrier','Chemical_tanker','General_cargo','Oil_tanker','Other_liquids_tankers','Refrigerated_bulk','Ro_Ro'),SHIP_DWT,
                                                                    Ship_Type%in%c('Container'),TEUCapacity)]
test0=test0[,unit:=fcase(Ship_Type%in%c('Bulk_carrier','Chemical_tanker','General_cargo','Oil_tanker','Other_liquids_tankers','Refrigerated_bulk','Ro_Ro'),'dwt',
                         Ship_Type%in%c('Container'),'teu')]
#-----写成函数 add ship size bin-----
test0=test0[,size_bin:=fcase(Ship_Type=='Bulk_carrier'&capacity%between%c(0,9999),1,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(10000,34999),2,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(35000,59999),3,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(60000,99999),4,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(10000,199999),5,
                             Ship_Type=='Bulk_carrier'&capacity>=200000,6,
                             Ship_Type=='Chemical_tanker'&capacity%between%c(0,4999),1,
                             Ship_Type=='Chemical_tanker'&capacity%between%c(5000,9999),2,
                             Ship_Type=='Chemical_tanker'&capacity%between%c(10000,19999),3,
                             Ship_Type=='Chemical_tanker'&capacity%between%c(20000,39999),4,
                             Ship_Type=='Chemical_tanker'&capacity>=40000,5,
                             Ship_Type=='Container'&capacity%between%c(0,999),1,
                             Ship_Type=='Container'&capacity%between%c(1000,1999),2,
                             Ship_Type=='Container'&capacity%between%c(2000,2999),3,
                             Ship_Type=='Container'&capacity%between%c(3000,4999),4,
                             Ship_Type=='Container'&capacity%between%c(5000,7999),5,
                             Ship_Type=='Container'&capacity%between%c(8000,11999),6,
                             Ship_Type=='Container'&capacity%between%c(12000,14499),7,
                             Ship_Type=='Container'&capacity%between%c(14500,19999),8,
                             Ship_Type=='Container'&capacity>=20000,9,
                             Ship_Type=='General_cargo'&capacity%between%c(0,4999),1,
                             Ship_Type=='General_cargo'&capacity%between%c(5000,9999),2,
                             Ship_Type=='General_cargo'&capacity%between%c(10000,19999),3,
                             Ship_Type=='General_cargo'&capacity>=20000,4,
                             Ship_Type=='Liquefied_gas_tanker'&capacity%between%c(0,49999),1,
                             Ship_Type=='Liquefied_gas_tanker'&capacity%between%c(50000,99999),2,
                             Ship_Type=='Liquefied_gas_tanker'&capacity%between%c(100000,199999),3,
                             Ship_Type=='Liquefied_gas_tanker'&capacity>=200000,4,
                             Ship_Type=='Oil_tanker'&capacity%between%c(0,4999),1,
                             Ship_Type=='Oil_tanker'&capacity%between%c(5000,9999),2,
                             Ship_Type=='Oil_tanker'&capacity%between%c(10000,19999),3,
                             Ship_Type=='Oil_tanker'&capacity%between%c(20000,59999),4,
                             Ship_Type=='Oil_tanker'&capacity%between%c(60000,79999),5,
                             Ship_Type=='Oil_tanker'&capacity%between%c(80000,119999),6,
                             Ship_Type=='Oil_tanker'&capacity%between%c(120000,199999),7,
                             Ship_Type=='Oil_tanker'&capacity>=200000,8,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(0,9999),1,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(10000,34999),2,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(35000,59999),3,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(60000,99999),4,
                             Ship_Type=='Bulk_carrier'&capacity%between%c(10000,199999),5,
                             Ship_Type=='Bulk_carrier'&capacity>=200000,6,
                             Ship_Type=='Other_liquids_tankers'&capacity%between%c(0,999),1,
                             Ship_Type=='Other_liquids_tankers'&capacity>=1000,2,
                             Ship_Type=='Ferry_pax_only'&capacity%between%c(0,299),1,
                             Ship_Type=='Ferry_pax_only'&capacity%between%c(300,999),2,
                             Ship_Type=='Ferry_pax_only'&capacity%between%c(1000,1999),3,
                             Ship_Type=='Ferry_pax_only'&capacity>=2000,4,
                             Ship_Type=='Curise'&capacity%between%c(0,1999),1,
                             Ship_Type=='Curise'&capacity%between%c(2000,9999),2,
                             Ship_Type=='Curise'&capacity%between%c(10000,59999),3,
                             Ship_Type=='Curise'&capacity%between%c(60000,99999),4,
                             Ship_Type=='Curise'&capacity%between%c(100000,149999),5,
                             Ship_Type=='Curise'&capacity>=150000,6,
                             Ship_Type=='Ferry_RoPax'&capacity%between%c(0,1999),1,
                             Ship_Type=='Ferry_RoPax'&capacity%between%c(2000,4999),2,
                             Ship_Type=='Ferry_RoPax'&capacity%between%c(5000,9999),3,
                             Ship_Type=='Ferry_RoPax'&capacity%between%c(10000,19999),4,
                             Ship_Type=='Ferry_RoPax'&capacity>=20000,5,
                             Ship_Type=='Refrigerated_bulk'&capacity%between%c(0,1999),1,
                             Ship_Type=='Refrigerated_bulk'&capacity%between%c(2000,5999),2,
                             Ship_Type=='Refrigerated_bulk'&capacity%between%c(6000,9999),3,
                             Ship_Type=='Refrigerated_bulk'&capacity>=10000,4,
                             Ship_Type=='Ro_Ro'&capacity%between%c(0,4999),1,
                             Ship_Type=='Ro_Ro'&capacity%between%c(5000,9999),2,
                             Ship_Type=='Ro_Ro'&capacity%between%c(10000,14999),3,
                             Ship_Type=='Ro_Ro'&capacity>=15000,4,
                             Ship_Type=='Vehicle'&capacity%between%c(0,9999),1,
                             Ship_Type=='Vehicle'&capacity%between%c(10000,19999),2,
                             Ship_Type=='Vehicle'&capacity>=20000,3,
                             Ship_Type=='Yacht'&capacity>=0,1,
                             Ship_Type=='Service_tug'&capacity>=0,1,
                             Ship_Type=='Miscellaneous_fishing'&capacity>=0,1,
                             Ship_Type=='Offshore'&capacity>=0,1,
                             Ship_Type=='Service_other'&capacity>=0,1,
                             Ship_Type=='Miscellaneous_other'&capacity>=0,1)]
#----calculate power fuel and emissions-----

#功率
AE_AB_Power=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')
test0=test0[,MEPower:=loadFactor*SHIP_ENGINE_POWER]
#注意船舶类型字段可能需要修改
test0=AE_AB_Power[,list(Ship_Type=ShipType,type_bin,size_bin,mode,ABPower,AEPower)][test0,on=.(Ship_Type,size_bin,mode)]


#燃油消耗
baseSFC=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_19_baseSFC.csv')

test0=baseSFC[,list(ME_engineType=EngineType,ME_fuelType=FuelType,gen,ME_SFCbase=SFCbase)][test0,on=.(ME_engineType,ME_fuelType,gen)]
test0=baseSFC[,list(AE_engineType=EngineType,AE_fuelType=FuelType,gen,AE_SFCbase=SFCbase)][test0,on=.(AE_engineType,AE_fuelType,gen)]
test0=baseSFC[,list(AB_engineType=EngineType,AB_fuelType=FuelType,gen,AB_SFCbase=SFCbase)][test0,on=.(AB_engineType,AB_fuelType,gen)]
test0=test0[,ME_SFC:=ME_SFCbase*(0.445*loadFactor^2-0.710*loadFactor+1.280)][,ME_FC:=ME_SFC*loadFactor*SHIP_ENGINE_POWER*hours]
test0=test0[loadFactor<0.07,ME_FC:=0] # IMO4表示小于0.07就不消耗燃油 
test0=test0[,AE_FC:=AE_SFCbase*AEPower*hours][,AB_FC:=AB_SFCbase*ABPower*hours]


#二氧化碳
CO2table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_45_CO2_EF.csv')
LLAtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO_LowLoadAdjustmentFactors.csv')
test0=LLAtable[test0,on='load_bin']
#test0=test0[,ME_fuelType:='MDO'][,AE_fuelType:='MDO'][,AB_fuelType:='MDO']
test0=CO2table[,list(ME_fuelType=FuelType,ME_CO2EF=CO2EF)][test0,on='ME_fuelType']#left_join
test0=CO2table[,list(AE_fuelType=FuelType,AE_CO2EF=CO2EF)][test0,on='AE_fuelType']
test0=CO2table[,list(AB_fuelType=FuelType,AB_CO2EF=CO2EF)][test0,on='AB_fuelType']
test0=test0[,ME_CO2:=ME_FC*ME_CO2EF*CO2_LLA][,AE_CO2:=AE_FC*AE_CO2EF][,AB_CO2:=AB_FC*AB_CO2EF]

#硫化物
SOxtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_47_SOx_EF.csv')
test0=SOxtable[,list(ME_fuelType=FuelType,ME_SOxEF=SOxEF)][test0,on='ME_fuelType']#left_join
test0=SOxtable[,list(AE_fuelType=FuelType,AE_SOxEF=SOxEF)][test0,on='AE_fuelType']
test0=SOxtable[,list(AB_fuelType=FuelType,AB_SOxEF=SOxEF)][test0,on='AB_fuelType']
test0=test0[,ME_SOx:=ME_FC*ME_SOxEF*SOx_LLA][,AE_SOx:=AE_FC*AE_SOxEF][,AB_SOx:=AB_FC*AB_SOxEF]

#黑炭

test0=test0[,BC_load_bin:=floor(loadFactor*10)+2][loadFactor<0.05,BC_load_bin:=1][loadFactor>=1,BC_load_bin:=11]
test0=test0[,stroke:=2]#假设二冲程
ME_BCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_63_BC_EF_forME.csv')
AE_AB_BCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_64_BC_EF_forAE.csv')
test0=ME_BCtable[,list(BC_load_bin,ME_engineType=EngineType,ME_fuelType=FuelType,stroke,ME_BC_EFf)][test0,on=.(BC_load_bin,ME_engineType,ME_fuelType,stroke)]
test0=test0[,ME_BC:=ME_FC*ME_BC_EFf*hours]
#IMO4中没有副机MDO等油耗的BC排放，covered by table 52 不明白。目前假设与锅炉的相同
test0=AE_AB_BCtable[,list(AE_engineType=EngineType,AE_fuelType=FuelType,AE_BC_EFe=AE_AB_BC_EFe)][test0,on=.(AE_engineType,AE_fuelType)]
test0=AE_AB_BCtable[,list(AB_engineType=EngineType,AB_fuelType=FuelType,AB_BC_EFe=AE_AB_BC_EFe)][test0,on=.(AB_engineType,AB_fuelType)]
test0=test0[,AE_BC:=AEPower*AE_BC_EFe*hours][,AB_BC:=ABPower*AB_BC_EFe*hours]

#氮化物
NOxtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_50_NOx_EF.csv')
test0=NOxtable[,list(ME_fuelType=FuelType,ME_engineType=EngineType,tier,ME_NOxEF=NOxEF)][test0,on=.(ME_engineType,ME_fuelType,tier)]#left_join
test0=NOxtable[,list(AE_fuelType=FuelType,AE_engineType=EngineType,tier,AE_NOxEF=NOxEF)][test0,on=.(AE_engineType,AE_fuelType,tier)]
test0=NOxtable[,list(AB_fuelType=FuelType,AB_engineType=EngineType,tier,AB_NOxEF=NOxEF)][test0,on=.(AB_engineType,AB_fuelType,tier)]
test0=test0[,ME_NOx:=MEPower*ME_NOxEF*NOx_LLA*hours][,AE_NOx:=AEPower*AE_NOxEF*hours][,AB_NOx:=ABPower*AB_NOxEF*hours]

#甲烷

CH4table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_Appendix_B_table_6_CH4_EF.csv')
test0=CH4table[,list(ME_fuelType=FuelType,ME_engineType=EngineType,ME_CH4EF=CH4EF)][test0,on=.(ME_engineType,ME_fuelType)]#left_join
test0=CH4table[,list(AE_fuelType=FuelType,AE_engineType=EngineType,AE_CH4EF=CH4EF)][test0,on=.(AE_engineType,AE_fuelType)]
test0=CH4table[,list(AB_fuelType=FuelType,AB_engineType=EngineType,AB_CH4EF=CH4EF)][test0,on=.(AB_engineType,AB_fuelType)]
test0=test0[,ME_CH4:=MEPower*ME_CH4EF*CH4_LLA*hours][,AE_CH4:=AEPower*AE_CH4EF*hours][,AB_CH4:=ABPower*AB_CH4EF*hours]


#一氧化碳

COtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_57_CO_EF.csv')
test0=COtable[,list(ME_fuelType=FuelType,ME_engineType=EngineType,ME_COEF=COEF)][test0,on=.(ME_engineType,ME_fuelType)]#left_join
test0=COtable[,list(AE_fuelType=FuelType,AE_engineType=EngineType,AE_COEF=COEF)][test0,on=.(AE_engineType,AE_fuelType)]
test0=COtable[,list(AB_fuelType=FuelType,AB_engineType=EngineType,AB_COEF=COEF)][test0,on=.(AB_engineType,AB_fuelType)]
test0=test0[,ME_CO:=MEPower*ME_COEF*CO_LLA*hours][,AE_CO:=AEPower*AE_COEF*hours][,AB_CO:=ABPower*AB_COEF*hours]

#一氧化二氮

N2Otable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_59_N2O_EF.csv')
test0=N2Otable[,list(ME_fuelType=FuelType,ME_engineType=EngineType,ME_N2OEF=N2OEF)][test0,on=.(ME_engineType,ME_fuelType)]#left_join
test0=N2Otable[,list(AE_fuelType=FuelType,AE_engineType=EngineType,AE_N2OEF=N2OEF)][test0,on=.(AE_engineType,AE_fuelType)]
test0=N2Otable[,list(AB_fuelType=FuelType,AB_engineType=EngineType,AB_N2OEF=N2OEF)][test0,on=.(AB_engineType,AB_fuelType)]
test0=test0[,ME_N2O:=MEPower*ME_N2OEF*N2O_LLA*hours][,AE_N2O:=AEPower*AE_N2OEF*hours][,AB_N2O:=ABPower*AB_N2OEF*hours]

#颗粒物PM10

PM10table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM10_EF.csv')
test0=PM10table[,list(ME_fuelType=FuelType,ME_engineType=EngineType,gen,ME_PM10EF=PM10EF)][test0,on=.(ME_engineType,ME_fuelType,gen)]#left_join
test0=PM10table[,list(AE_fuelType=FuelType,AE_engineType=EngineType,gen,AE_PM10EF=PM10EF)][test0,on=.(AE_engineType,AE_fuelType,gen)]
test0=PM10table[,list(AB_fuelType=FuelType,AB_engineType=EngineType,gen,AB_PM10EF=PM10EF)][test0,on=.(AB_engineType,AB_fuelType,gen)]
test0=test0[,ME_PM10:=MEPower*ME_PM10EF*PM10_LLA*hours][,AE_PM10:=AEPower*AE_PM10EF*hours][,AB_PM10:=ABPower*AB_PM10EF*hours]

#颗粒物PM2.5

PM2.5table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM2.5_EF.csv')
test0=PM2.5table[,list(ME_fuelType=FuelType,ME_engineType=EngineType,gen,ME_PM2.5EF=PM2.5EF)][test0,on=.(ME_engineType,ME_fuelType,gen)]#left_join
test0=PM2.5table[,list(AE_fuelType=FuelType,AE_engineType=EngineType,gen,AE_PM2.5EF=PM2.5EF)][test0,on=.(AE_engineType,AE_fuelType,gen)]
test0=PM2.5table[,list(AB_fuelType=FuelType,AB_engineType=EngineType,gen,AB_PM2.5EF=PM2.5EF)][test0,on=.(AB_engineType,AB_fuelType,gen)]
test0=test0[,ME_PM2.5:=MEPower*ME_PM2.5EF*PM2.5_LLA*hours][,AE_PM2.5:=AEPower*AE_PM2.5EF*hours][,AB_PM2.5:=ABPower*AB_PM2.5EF*hours]


#非甲烷挥发性有机化合物NMVOC

NMVOCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_61_NMVOC_EF.csv')
test0=NMVOCtable[,list(ME_fuelType=FuelType,ME_engineType=EngineType,ME_NMVOCEF=NMVOCEF)][test0,on=.(ME_engineType,ME_fuelType)]#left_join
test0=NMVOCtable[,list(AE_fuelType=FuelType,AE_engineType=EngineType,AE_NMVOCEF=NMVOCEF)][test0,on=.(AE_engineType,AE_fuelType)]
test0=NMVOCtable[,list(AB_fuelType=FuelType,AB_engineType=EngineType,AB_NMVOCEF=NMVOCEF)][test0,on=.(AB_engineType,AB_fuelType)]
test0=test0[,ME_NMVOC:=MEPower*ME_NMVOCEF*NMVOC_LLA*hours][,AE_NMVOC:=AEPower*AE_NMVOCEF*hours][,AB_NMVOC:=ABPower*AB_NMVOCEF*hours]


sum(test0$ME_FC)/1000/1000

sum(test0$ME_FC)/1000/1000/((ship1[nrow(ship1),MEAccFOCons]-ship1[1,MEAccFOCons])/1000)

fwrite(test0,'/Users/mizexin/AIS/out/badayan_inventroy.csv')

library('ggplot2')
library('ggthemes')
library('ggpubr')

p=ggplot(ship1[,list(hour_FC=sum(ME_FC_kwh)),yhour])+
  geom_path(aes(x=yhour,y=hour_FC),col='red')+
  geom_path(data=test0[,list(hour_FC=sum(ME_FC)/1000/1000),yhour],aes(x=yhour,y=hour_FC),col='blue')+ theme_bw()+rremove('grid')
p=ggpar(p, 
        title = "八打雁每小时实油耗与估测油耗比较（单位：吨），红色为实测油耗，蓝色为估测油耗",
        xlab ="年小时", 
        ylab = "油耗（吨）",
        legend.title = "红色为实测油耗，蓝色为估测油耗"
)

p
