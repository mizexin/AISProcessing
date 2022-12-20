library('data.table')
library('plyr')
library('dplyr')
library('stringr')
library('lubridate')

AE_AB_Power=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')
CO2_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_45_CO2_EF.csv')
SO2_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_47_SOx_EF.csv')
NO2_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_50_NOx_EF.csv')
PM2.5_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM2.5_EF.csv')
PM10_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM10_EF.csv')
CH4_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_Appendix_B_table_6_CH4_EF.csv')
CO_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_57_CO_EF.csv')
N2O_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_59_N2O_EF.csv')
NMVOC_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_61_NMVOC_EF.csv')
BC_ME_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_63_BC_EF_forME.csv')
BC_AE_EF=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_64_BC_EF_forAE.csv')

# 读取文件 把mmsi调整至第一列
hifleet_containers=fread("~/R/AIS_Processing/carbon-main/data/IMO4_table/hifleetships_brief_container_for_student.csv")[,.SD[1,],mmsi][,mmsi:=as.character(mmsi)]
# 提取原始数据，该数据以船舶stop的形式与相应的港口和国家进行了关联
# 读取原始文件路径 文件列表
filepaths=dir('~/AIS/2019_year_container/',full.names = TRUE)
# 提取文件名中的mmsi
mmsis=data.table(dir('~/AIS/2019_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi 
# 删除字符串开头和结尾的空格
mmsis=str_trim(mmsis)
# shipstopfiles0 : 将路径和mmsi组成名为shipstopfiles0的数据表 
shipstopfiles0=data.table(filepath=filepaths,mmsi=mmsis)
# shipstopfiles: shipstopfiles0和船舶信息以mmsi进行匹配 有缺失数据
shipstopfiles=left_join(shipstopfiles0,hifleet_containers,'mmsi')#部分船舶字段数据缺失
# 新增一列teu 按照1-9进行分级 分别对应
shipstopfiles=shipstopfiles[,teu:=as.integer(teu)][,sizegroup:=cut(teu,breaks=c(0,1000,2000,3000,5000,8000,12000,14500,20000,30000),labels=seq(1,9),right=FALSE)];shipstopfiles
# shipn： 文件行数
shipn=nrow(shipstopfiles)
# aship0：读取shipstopfiles中第一个文件 速度小于300
aship0=fread(shipstopfiles[4]$filepath)[speed<300]
# aship0与shipstopfiles进行mmsi匹配
aship=left_join(aship0,shipstopfiles[,mmsi:=as.integer(mmsi)],'mmsi')
# 新增date列为timegroup列
aship=aship[,date:=as.POSIXct(time,origin="1970-01-01")][,timegroup:=cut(date,breaks='5 min')]
aship=aship[,month:=month(aship$date)]
# 新增speedgroup列 速度等级分为四段
aship=aship[,speedgroup:=cut(speed,breaks=c(0,10,30,50,400),labels=seq(1,4),right=FALSE)];aship

test0=test0[,loadFactor:=((speed/10)/(SHIP_SPEED/0.94))^3][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]

test0[,N:=NULL];test0$vesselType=NULL;test0$lengthOverall=NULL;test0$i.N=NULL

test0=test0[,ME_engineType:='SSD'][,AE_engineType:='Auxiliary_Engines'][,AB_engineType:='Steam_Boiler']
test0=test0[,ME_fuelType:='MDO'][,AE_fuelType:='MDO'][,AB_fuelType:='MDO']
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