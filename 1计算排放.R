library(readxl)
library(hrbrthemes)
library(lubridate)
library(mapdeck)
library(dplyr)
library(data.table)
library(circular)
library(janitor)
library(ggplot2)
library(doSNOW)
library(tcltk)
library(doParallel)
library(stats)

setDTthreads(threads = 10)

AIS_year ='2020'

source('/Users/mizexin/R/AISProcessing/functions2020-copy.R')

ships_static_data              =  fread('/Users/mizexin/论文数据/FIX_ships_static_data.csv')%>%rename(.,'main_eng_type'= 'engine_type','stroke' = 'engine_stroke_type')
ships_static_data[grep(pattern = "Container",ship_type) ,ship_type:='Container']
AE_AB_Power                    = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')#%>%clean_names()
baseSFC                        = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_19_baseSFC.csv')
CO2table                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_45_CO2_EF.csv')
LLAtable                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO_LowLoadAdjustmentFactors.csv')
SOxtable                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_47_SOx_EF.csv')
ME_BCtable                     = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_63_BC_EF_forME.csv') %>% distinct()#重复行导致报错
AE_AB_BCtable                  = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_64_BC_EF_forAE.csv')
NOxtable                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_50_NOx_EF.csv')
CH4table                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_Appendix_B_table_6_CH4_EF.csv')
COtable                        = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_57_CO_EF.csv')
N2Otable                       = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_59_N2O_EF.csv')
PM10table                      = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_52_PM10_EF.csv')
PM2.5table                     = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_52_PM2.5_EF.csv')
NMVOCtable                     = fread('~/R/AISProcessing/carbon-main/data/IMO4_table/IMO4_table_61_NMVOC_EF.csv')

# ships AIS data ctalog
dir       <-  paste("~/AIS/",AIS_year,"_year_container",sep = '')
file_list <-  list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num  <- length(file_list)

cl=makeCluster(10)
registerDoParallel(cl)

x<-foreach(i=1:file_num,.packages=c('data.table','dplyr','lubridate','circular')) %dopar% {
             agroup=fread(paste(file_list[i],sep = ''),select = c('mmsi','time','speed','lon','lat'),
                          # agroup=fread(paste("/Users/mizexin/AIS/2020_year_container/219036000.csv",sep = '')
                          #              ,select = c('mmsi','time','speed','lon','lat'),
                          col.names = c('mmsi','time','sog','lon','lat'))
             mmsis=agroup[,.N,mmsi]$mmsi
             k=length(mmsis)

              if (nrow(agroup) >= 2 ) {
              
                for (j in seq(1,k)) {
                  
                 ammsi=mmsis[j] #单跑改k j
                 
                 aship=agroup[mmsi==ammsi]
                 
                 aship=distinct(aship)[,list(mmsi,time,sog,lon=lon/1000000,lat=lat/1000000)]
                 
                 aship=data.table(addDateTime(aship))
                 
                 aship=aship[order(aship$datetime),]
                 
                 lines=getLines(aship)
                 
                 speedlines=getLineSpeed(lines)
                 
                 abnormallines=getAbnormalLine(speedlines) # 通过调整这个参数了异常线段，超过limit1且平均航速小于3节，或者 大雨limit2 都为异常。另一个条件是航速超过37.5也为异常
                 # abnormallines[isabnormal>0]#提取异常线段中的开始和结束时间
                 ablines=abnormallines[isabnormal>0][,list(datetime1,datetime2)] #提取异常线段两个端点的时间
              
                 aship=add_day_point(aship[,list(mmsi,time=time1,sog=sog1,lon=lon1,lat=lat1,datetime=datetime1)])

                 # 去除异常线段中的插值，之后就可以按照原来的程序计算排放了
                 aship=aship[!(isaddedday>0&inrange(datetime,ablines$datetime1,ablines$datetime2))]

                 # aship=aship[!(inrange(datetime,ablines$datetime1,ablines$datetime2))]
                 
                 aship=AddInfo(aship)
                 
                 aship=aship[!((durhour>=72&avgspeed<3)|durhour>=240|avgspeed>=37.5)]
                 #aship[durhour>400]
                 
                 aship=SplitTimeFormat(aship)
                 
                 aship=aship[lon<=180][sog>=0][yyear == AIS_year][,lon:=as.numeric(lon)][,lat:=as.numeric(lat)]

                 aship=aship[,speedrate:=avgspeed/20] # 设计航速 20
                 
                 # 5海里以上 超过两倍的平均航速
                 
                 aship=aship[!(distnm>=5&speedrate>=2)]
                 
                 # aship=aship[!(avgspeed < 3 & durhour > 72)]
                 
                 aship=aship[,hours:= durhour][,speed:=avgspeed]
                 
                 aship=left_join(aship,ships_static_data,by='mmsi')
                 
                 test0=aship[,loadFactor:=round((0.62)^0.66*((speed)/(service_speed/0.94))^3,2)/0.867/0.917][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]
                 
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
                 test0[,98:132]=test0[,98:132]%>%round(.,2)
                 
                
                 # fwrite(cbind(test0),paste('/Volumes/Samsung\ SSD/AIS/',AIS_year,'EM/',ammsi,'.gz',sep = ''))
               }
             }
           }
stopCluster(cl)
