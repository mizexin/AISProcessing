# imoport packages
library(readxl)
library(hrbrthemes)
library(lubridate)
hrbrthemes::import_roboto_condensed()
library(mapdeck)
library(data.table)
library(dplyr)
library(sf)
library(extrafont)
library(tidycovid19)
library(data.table)
library(circular)
library(doParallel)
library(janitor)
library(ggplot2)

# Read tables and ships static data source
ships_static_data = fread('/Users/mizexin/Desktop/论文数据/FIX_ships_static_data.csv')%>%rename(.,'main_eng_type'= 'engine_type','stroke' = 'engine_stroke_type')
ships_static_data[grep(pattern = "Container",ship_type) ,ship_type:='Container']
AE_AB_Power = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')%>%clean_names()
AE_AB_Power = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')
baseSFC = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_19_baseSFC.csv')
CO2table = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_45_CO2_EF.csv')
LLAtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO_LowLoadAdjustmentFactors.csv')
SOxtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_47_SOx_EF.csv')
ME_BCtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_63_BC_EF_forME.csv') %>% distinct()#重复行导致报错
AE_AB_BCtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_64_BC_EF_forAE.csv')
NOxtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_50_NOx_EF.csv')
CH4table = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_Appendix_B_table_6_CH4_EF.csv')
COtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_57_CO_EF.csv')
N2Otable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_59_N2O_EF.csv')
PM10table = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM10_EF.csv')
PM2.5table = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM2.5_EF.csv')
NMVOCtable = fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_61_NMVOC_EF.csv')

# distance and time functions
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
# ships AIS data ctalog
dir = "~/AIS/2019_year_container" 
file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num <- length(file_list)


agroup=fread(paste("/Users/mizexin/AIS/2019_year_container/255806089.csv",sep = ''),
               col.names = c('mmsi','time','status','lon','lat','speed','cog','head'))#同名文件组成一个？
  mmsis=agroup[,.N,mmsi]$mmsi
  k=length(mmsis)
  if(nrow(agroup) == 1)
    next
  # for (j in seq(1,k))
  # {
    print(paste('j=',j))
    #   
    
    #航速筛选计划 1 船舶速度应该小于最大航速的1.2倍
    #             2 两点的平均速度 应该和位移除以时间的速度相 差不多
    
    ammsi=mmsis[k] #单跑改k
    aship=agroup[mmsi==ammsi]
    aship=distinct(aship)
    aship=data.table(addDateTime(aship))[,list(mmsi,time,status,lon=lon/1000000,lat=lat/1000000,sod=speed,cog,head,datetime)]
    # lof方法处理明显异常值
    # lof_score = Rlof::lof(aship[,c('lon','lat')], k = 3, cores = 10)
    # aship$lof_score = round(lof_score,2)
    
    aship=aship[order(aship$datetime),][,timegroup:=ymd_hms(cut(datetime,breaks='5 min'))]
    aship=aship[,hour:=lubridate::hour(timegroup)][,day:=lubridate::day(timegroup)][,week:=lubridate::week(timegroup)][,month:=lubridate::month(timegroup)][,yyear:=lubridate::year(timegroup)][,yday:=yday(timegroup)]%>%distinct(timegroup, .keep_all = TRUE)
    
    aship=aship[lon<=180][sod>=0][yyear == 2019][,lon:=as.numeric(lon)][,lat:=as.numeric(lat)]
    aship=aship[,yhour:=hour+(yday-1)*24]
    aship=aship[,dur:=shift(datetime,-1)-datetime][,dur:=as.integer(dur)]
    aship=aship[,lon2:=shift(lon,-1)][,lat2:=shift(lat,-1)][!is.na(lon2)][,speedid:=round(as.numeric(sod/10))]#
    aship=aship[,dist:=round(distance(lon,lat,lon2,lat2))]
    
    # qplot(aship$lon,aship$lat)
    aship=aship[,avgspeed:=round(dist/1852*3600/dur,2)][,speedrate:=avgspeed/20] # 设计航速 20
    # 5海里以上 超过两倍的平均航速
    # aship=aship[-which(dist>5*1852&speedrate>2)]
    aship=aship[!(dist>5*1852&speedrate>2)]
    
    aship=aship[,gap_hours:=unclass(shift(datetime,-1)-datetime)/3600]
    
    aship=aship[!(avgspeed < 3 & gap_hours > 72)]
    
    aship=aship[gap_hours >24,speedid:=avgspeed]
    
    ship0=aship[,list(.N,hours=round(sum(dur)/3600,2),distnm=round(sum(dist)/1852,2)),list(mmsi,speed=speedid,hour,yhour,day,week,month,yday,yyear,avgspeed,gap_hours)]
    ship0=left_join(ship0,ships_static_data,by='mmsi')
    
    
    # test0=ship0[,loadFactor:=round(((speed/10)/(service_speed/0.94))^3,2)/0.867/0.917][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
    # 
    test0=ship0[,loadFactor:=round((0.62)^0.66*((speed)/(service_speed/0.94))^3,2)/0.867/0.917][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
    #test0=ship0[,loadFactor:=round(((speed)/(service_speed/0.94))^3,2)][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
    
    # 没有实际吃水 直接取值了
    test0=test0[speed<=service_speed*1.2]
    
    # 辅机锅炉类型定义 MDO
    test0=test0[,ae_eng_type:='Auxiliary_Engines'][,ab_eng_type:='Steam_Boiler']
    test0=test0[,ae_fuel_type:='MDO'][,ab_fuel_type:='MDO']
    # 对数据进行Gen年代，Tier等级，Mode航行模式的分类 /10
    test0=test0[,gen:=fcase(year <1984,1,
                            year%between%c(1984,2000),2,
                            year>=2001,3)]
    test0=test0[,tier:=fcase(year<=2000,0,
                             year%between%c(2001,2010),1,
                             year%between%c(2011,2015),2,
                             year>=2016,3)]
    test0=test0[,mode:=fcase(speed>=0&speed<1,'berth',
                             speed>=1&speed<3,'anchor',
                             speed>=3&loadFactor<=0.2,'manoeuvring',
                             speed>=3&loadFactor>=0.2&loadFactor<0.65,'sea',
                             loadFactor>=0.65,'sea')]
    
    #注：这个Liquefied_gas_tanker船舶需要进一步用cbm单位，目前用gt代替。
    
    test0=test0[,capacity:=fcase(ship_type%in%c('Bulk_carrier','Chemical_tanker','General_cargo',
                                                'Oil_tanker','Other_liquids_tankers','Refrigerated_bulk',
                                                'Ro_Ro',"Miscellaneous_other","Vehicle","Offshore",
                                                "Service_tug", "Curise","Service_other",
                                                "Miscellaneous_fishing" ,"Ferry_pax_only"),deadweight,
                                 ship_type%in%c('Container'),teu,
                                 ship_type%in%c('Liquefied_gas_tanker'),gt)]
    test0=test0[,unit:=fcase(ship_type%in%c('Bulk_carrier','Chemical_tanker','General_cargo',
                                            'Oil_tanker','Other_liquids_tankers','Refrigerated_bulk',
                                            'Ro_Ro',"Miscellaneous_other","Vehicle",
                                            "Offshore","Service_tug", "Curise","Service_other",
                                            "Miscellaneous_fishing","Ferry_pax_only"),'dwt',
                             ship_type%in%c('Container'),'teu',
                             ship_type%in%c('Liquefied_gas_tanker'),'gt')]
    
    #-----写成函数 add ship size bin-----
    test0=test0[,size_bin:=fcase(ship_type=='Bulk_carrier'&capacity%between%c(0,9999),1,
                                 ship_type=='Bulk_carrier'&capacity%between%c(10000,34999),2,
                                 ship_type=='Bulk_carrier'&capacity%between%c(35000,59999),3,
                                 ship_type=='Bulk_carrier'&capacity%between%c(60000,99999),4,
                                 ship_type=='Bulk_carrier'&capacity%between%c(10000,199999),5,
                                 ship_type=='Bulk_carrier'&capacity>=200000,6,
                                 ship_type=='Chemical_tanker'&capacity%between%c(0,4999),1,
                                 ship_type=='Chemical_tanker'&capacity%between%c(5000,9999),2,
                                 ship_type=='Chemical_tanker'&capacity%between%c(10000,19999),3,
                                 ship_type=='Chemical_tanker'&capacity%between%c(20000,39999),4,
                                 ship_type=='Chemical_tanker'&capacity>=40000,5,
                                 ship_type=='Container'&capacity%between%c(0,999),1,
                                 ship_type=='Container'&capacity%between%c(1000,1999),2,
                                 ship_type=='Container'&capacity%between%c(2000,2999),3,
                                 ship_type=='Container'&capacity%between%c(3000,4999),4,
                                 ship_type=='Container'&capacity%between%c(5000,7999),5,
                                 ship_type=='Container'&capacity%between%c(8000,11999),6,
                                 ship_type=='Container'&capacity%between%c(12000,14499),7,
                                 ship_type=='Container'&capacity%between%c(14500,19999),8,
                                 ship_type=='Container'&capacity>=20000,9,
                                 ship_type=='General_cargo'&capacity%between%c(0,4999),1,
                                 ship_type=='General_cargo'&capacity%between%c(5000,9999),2,
                                 ship_type=='General_cargo'&capacity%between%c(10000,19999),3,
                                 ship_type=='General_cargo'&capacity>=20000,4,
                                 ship_type=='Liquefied_gas_tanker'&capacity%between%c(0,49999),1,
                                 ship_type=='Liquefied_gas_tanker'&capacity%between%c(50000,99999),2,
                                 ship_type=='Liquefied_gas_tanker'&capacity%between%c(100000,199999),3,
                                 ship_type=='Liquefied_gas_tanker'&capacity>=200000,4,
                                 ship_type=='Oil_tanker'&capacity%between%c(0,4999),1,
                                 ship_type=='Oil_tanker'&capacity%between%c(5000,9999),2,
                                 ship_type=='Oil_tanker'&capacity%between%c(10000,19999),3,
                                 ship_type=='Oil_tanker'&capacity%between%c(20000,59999),4,
                                 ship_type=='Oil_tanker'&capacity%between%c(60000,79999),5,
                                 ship_type=='Oil_tanker'&capacity%between%c(80000,119999),6,
                                 ship_type=='Oil_tanker'&capacity%between%c(120000,199999),7,
                                 ship_type=='Oil_tanker'&capacity>=200000,8,
                                 ship_type=='Bulk_carrier'&capacity%between%c(0,9999),1,
                                 ship_type=='Bulk_carrier'&capacity%between%c(10000,34999),2,
                                 ship_type=='Bulk_carrier'&capacity%between%c(35000,59999),3,
                                 ship_type=='Bulk_carrier'&capacity%between%c(60000,99999),4,
                                 ship_type=='Bulk_carrier'&capacity%between%c(10000,199999),5,
                                 ship_type=='Bulk_carrier'&capacity>=200000,6,
                                 ship_type=='Other_liquids_tankers'&capacity%between%c(0,999),1,
                                 ship_type=='Other_liquids_tankers'&capacity>=1000,2,
                                 ship_type=='Ferry_pax_only'&capacity%between%c(0,299),1,
                                 ship_type=='Ferry_pax_only'&capacity%between%c(300,999),2,
                                 ship_type=='Ferry_pax_only'&capacity%between%c(1000,1999),3,
                                 ship_type=='Ferry_pax_only'&capacity>=2000,4,
                                 ship_type=='Curise'&capacity%between%c(0,1999),1,
                                 ship_type=='Curise'&capacity%between%c(2000,9999),2,
                                 ship_type=='Curise'&capacity%between%c(10000,59999),3,
                                 ship_type=='Curise'&capacity%between%c(60000,99999),4,
                                 ship_type=='Curise'&capacity%between%c(100000,149999),5,
                                 ship_type=='Curise'&capacity>=150000,6,
                                 ship_type=='Ferry_RoPax'&capacity%between%c(0,1999),1,
                                 ship_type=='Ferry_RoPax'&capacity%between%c(2000,4999),2,
                                 ship_type=='Ferry_RoPax'&capacity%between%c(5000,9999),3,
                                 ship_type=='Ferry_RoPax'&capacity%between%c(10000,19999),4,
                                 ship_type=='Ferry_RoPax'&capacity>=20000,5,
                                 ship_type=='Refrigerated_bulk'&capacity%between%c(0,1999),1,
                                 ship_type=='Refrigerated_bulk'&capacity%between%c(2000,5999),2,
                                 ship_type=='Refrigerated_bulk'&capacity%between%c(6000,9999),3,
                                 ship_type=='Refrigerated_bulk'&capacity>=10000,4,
                                 ship_type=='Ro_Ro'&capacity%between%c(0,4999),1,
                                 ship_type=='Ro_Ro'&capacity%between%c(5000,9999),2,
                                 ship_type=='Ro_Ro'&capacity%between%c(10000,14999),3,
                                 ship_type=='Ro_Ro'&capacity>=15000,4,
                                 ship_type=='Vehicle'&capacity%between%c(0,9999),1,
                                 ship_type=='Vehicle'&capacity%between%c(10000,19999),2,
                                 ship_type=='Vehicle'&capacity>=20000,3,
                                 ship_type=='Yacht'&capacity>=0,1,
                                 ship_type=='Service_tug'&capacity>=0,1,
                                 ship_type=='Miscellaneous_fishing'&capacity>=0,1,
                                 ship_type=='Offshore'&capacity>=0,1,
                                 ship_type=='Service_other'&capacity>=0,1,
                                 ship_type=='Miscellaneous_other'&capacity>=0,1)]
    
    #--------------------------------------------------------------------------------
    test0=test0[,MEPower:=loadFactor*total_kw_main_eng]#####
    #注意船舶类型字段可能需要修改
    test0=AE_AB_Power[,list(ship_type=ShipType,type_bin,size_bin,mode,ABPower,AEPower)][test0,on=.(ship_type,size_bin,mode)]
    
    #燃油消耗
    
    test0=baseSFC[,list(main_eng_type=EngineType,main_fuel_type=FuelType,gen,ME_SFCbase=SFCbase)][test0,on=.(main_eng_type,main_fuel_type,gen)]
    test0=baseSFC[,list(ae_eng_type=EngineType,ae_fuel_type=FuelType,gen,AE_SFCbase=SFCbase)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
    test0=baseSFC[,list(ab_eng_type=EngineType,ab_fuel_type=FuelType,gen,AB_SFCbase=SFCbase)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
    test0=test0[,ME_SFC:=ME_SFCbase*(0.445*loadFactor^2-0.710*loadFactor+1.280)][,ME_FC:=ME_SFC*loadFactor*total_kw_main_eng*hours]
    test0=test0[loadFactor<0.07,ME_FC:=0] # IMO4表示小于0.07就不消耗燃油 
    test0=test0[,AE_FC:=AE_SFCbase*AEPower*hours][,AB_FC:=AB_SFCbase*ABPower*hours]
    
    
    #二氧化碳
    
    test0=LLAtable[test0,on='load_bin']
    #test0=test0[,ME_fuelType:='MDO'][,AE_fuelType:='MDO'][,AB_fuelType:='MDO']
    test0=CO2table[,list(main_fuel_type=FuelType,ME_CO2EF=CO2EF)][test0,on='main_fuel_type']#left_join
    test0=CO2table[,list(ae_fuel_type=FuelType,AE_CO2EF=CO2EF)][test0,on='ae_fuel_type']
    test0=CO2table[,list(ab_fuel_type=FuelType,AB_CO2EF=CO2EF)][test0,on='ab_fuel_type']
    test0=test0[,ME_CO2:=ME_FC*ME_CO2EF*CO2_LLA][,AE_CO2:=AE_FC*AE_CO2EF][,AB_CO2:=AB_FC*AB_CO2EF]
    
    #硫化物
    test0=SOxtable[,list(main_fuel_type=FuelType,ME_SOxEF=SOxEF)][test0,on='main_fuel_type']#left_join
    test0=SOxtable[,list(ae_fuel_type=FuelType,AE_SOxEF=SOxEF)][test0,on='ae_fuel_type']
    test0=SOxtable[,list(ab_fuel_type=FuelType,AB_SOxEF=SOxEF)][test0,on='ab_fuel_type']
    test0=test0[,ME_SOx:=ME_FC*ME_SOxEF*SOx_LLA][,AE_SOx:=AE_FC*AE_SOxEF][,AB_SOx:=AB_FC*AB_SOxEF]
    
    #黑炭
    
    test0=test0[,BC_load_bin:=floor(loadFactor*10)+2][loadFactor<0.05,BC_load_bin:=1][loadFactor>=1,BC_load_bin:=11]
    #test0=test0[,stroke:=2]#假设二冲程
    test0=ME_BCtable[,list(BC_load_bin,main_eng_type=EngineType,main_fuel_type=FuelType,stroke,ME_BC_EFf)][test0,on=.(BC_load_bin,main_eng_type,main_fuel_type,stroke)]
    test0=test0[,ME_BC:=ME_FC*ME_BC_EFf*hours]
    #IMO4中没有副机MDO等油耗的BC排放，covered by table 52 不明白。目前假设与锅炉的相同
    test0=AE_AB_BCtable[,list(ae_eng_type=EngineType,ae_fuel_type=FuelType,AE_BC_EFe=AE_AB_BC_EFe)][test0,on=.(ae_eng_type,ae_fuel_type)]
    test0=AE_AB_BCtable[,list(ab_eng_type=EngineType,ab_fuel_type=FuelType,AB_BC_EFe=AE_AB_BC_EFe)][test0,on=.(ab_eng_type,ab_fuel_type)]
    test0=test0[,AE_BC:=AEPower*AE_BC_EFe*hours][,AB_BC:=ABPower*AB_BC_EFe*hours]
    
    #氮化物
    test0=NOxtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,tier,ME_NOxEF=NOxEF)][test0,on=.(main_fuel_type,main_eng_type,tier)]#left_join
    test0=NOxtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,tier,AE_NOxEF=NOxEF)][test0,on=.(ae_eng_type,ae_fuel_type,tier)]
    test0=NOxtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,tier,AB_NOxEF=NOxEF)][test0,on=.(ab_fuel_type,ab_eng_type,tier)]
    test0=test0[,ME_NOx:=MEPower*ME_NOxEF*NOx_LLA*hours][,AE_NOx:=AEPower*AE_NOxEF*hours][,AB_NOx:=ABPower*AB_NOxEF*hours]
    
    #甲烷
    
    test0=CH4table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_CH4EF=CH4EF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
    test0=CH4table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_CH4EF=CH4EF)][test0,on=.(ae_eng_type,ae_fuel_type)]
    test0=CH4table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_CH4EF=CH4EF)][test0,on=.(ab_eng_type,ab_fuel_type)]
    test0=test0[,ME_CH4:=MEPower*ME_CH4EF*CH4_LLA*hours][,AE_CH4:=AEPower*AE_CH4EF*hours][,AB_CH4:=ABPower*AB_CH4EF*hours]
    
    
    #一氧化碳
    
    test0=COtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_COEF=COEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
    test0=COtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_COEF=COEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
    test0=COtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_COEF=COEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
    test0=test0[,ME_CO:=MEPower*ME_COEF*CO_LLA*hours][,AE_CO:=AEPower*AE_COEF*hours][,AB_CO:=ABPower*AB_COEF*hours]
    
    #一氧化二氮
    
    test0=N2Otable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_N2OEF=N2OEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
    test0=N2Otable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_N2OEF=N2OEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
    test0=N2Otable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_N2OEF=N2OEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
    test0=test0[,ME_N2O:=MEPower*ME_N2OEF*N2O_LLA*hours][,AE_N2O:=AEPower*AE_N2OEF*hours][,AB_N2O:=ABPower*AB_N2OEF*hours]
    
    #颗粒物PM10
    
    test0=PM10table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,gen,ME_PM10EF=PM10EF)][test0,on=.(main_eng_type,main_fuel_type,gen)]#left_join
    test0=PM10table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,gen,AE_PM10EF=PM10EF)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
    test0=PM10table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,gen,AB_PM10EF=PM10EF)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
    test0=test0[,ME_PM10:=MEPower*ME_PM10EF*PM10_LLA*hours][,AE_PM10:=AEPower*AE_PM10EF*hours][,AB_PM10:=ABPower*AB_PM10EF*hours]
    
    #颗粒物PM2.5
    
    test0=PM2.5table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,gen,ME_PM2.5EF=PM2.5EF)][test0,on=.(main_eng_type,main_fuel_type,gen)]#left_join
    test0=PM2.5table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,gen,AE_PM2.5EF=PM2.5EF)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
    test0=PM2.5table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,gen,AB_PM2.5EF=PM2.5EF)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
    test0=test0[,ME_PM2.5:=MEPower*ME_PM2.5EF*PM2.5_LLA*hours][,AE_PM2.5:=AEPower*AE_PM2.5EF*hours][,AB_PM2.5:=ABPower*AB_PM2.5EF*hours]
    
    
    #非甲烷挥发性有机化合物NMVOC
    
    test0=NMVOCtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_NMVOCEF=NMVOCEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
    test0=NMVOCtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_NMVOCEF=NMVOCEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
    test0=NMVOCtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_NMVOCEF=NMVOCEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
    test0=test0[,ME_NMVOC:=MEPower*ME_NMVOCEF*NMVOC_LLA*hours][,AE_NMVOC:=AEPower*AE_NMVOCEF*hours][,AB_NMVOC:=ABPower*AB_NMVOCEF*hours]
    test0[,92:126]=test0[,92:126]%>%round(.,2)
    

  # }
}

# total_me_fc = round(sum(test0$ME_FC)/1000000,2)
# total_ab_fc = round(sum(test0$AB_FC)/1000000,2)
# tota_ae_fc = round(sum(test0$AE_FC)/1000000,2)
# distnm = round(sum(sumtest0$distnm),2)
# tota_fc = round(sum(total_me_fc,total_ab_fc,tota_ae_fc),2)
# total_distence=sum(test0$distnm)
# distnm = round(sum(sum(test0$distnm),2))
# print(distnm)
# print(tota_fc)