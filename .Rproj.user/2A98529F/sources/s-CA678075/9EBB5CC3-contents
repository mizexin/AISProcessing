#calculate the emissions of a single ship 
library('data.table')
library('dplyr')
library('stringr')
library(lubridate)
library('plyr')
# 读取文件 把mmsi调整至第一列
hifleet_containers=fread("~/AIS/hifleetships_brief_container_for_student.csv")[,.SD[1,],mmsi][,mmsi:=as.character(mmsi)]

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
aship0=fread(shipstopfiles[3]$filepath)[speed<300]
# aship0与shipstopfiles进行mmsi匹配
aship=left_join(aship0,shipstopfiles[,mmsi:=as.integer(mmsi)],'mmsi')
# 新增date列为timegroup列
aship=aship[,date:=as.POSIXct(time,origin="1970-01-01")][,timegroup:=cut(date,breaks='5 min')]
aship=aship[,month:=month(aship$date)]
# 新增speedgroup列 速度等级分为四段
aship=aship[,speedgroup:=cut(speed,breaks=c(0,10,30,50,400),labels=seq(1,4),right=FALSE)];aship
# 
# 0.867 for Weather Correction Factor 
# 0.917 for Fouling Correction Factor P78 
# Most Container weather correction factor is 0.917 ｜ Fouling correction factor are all the same
aship=aship[,powerRate:=(speed/10/service_speed/0.94)^3/0.867/0.917]
aship=aship[,aPower:=powerRate*eng_total_kw]
# Container size 1 weather correction factor is 0.909 
aship=aship[sizegroup%in%c(1),powerRate:=(speed/10/service_speed/0.94)^3/0.909/0.917]
# 0.75 for Speed Power Correction Factor size 8-9 P79
aship=aship[sizegroup%in%c(8,9),powerRate:=0.75*(speed/10/service_speed/0.94)^3/0.867/0.917]
# a ship is operating in one of five defined phases
aship=aship[powerRate<=0.07,aPower:=0]
aship=aship[speed<=10,mode:="berth"]
aship=aship[speed>10&speed<=30,mode:="anchor"]
aship=aship[speed>30&speed<=80,mode:="maneuvering"]#4th imo ghg 50
aship=aship[speed>80&powerRate<=0.65,mode:="slow"]
aship=aship[speed>80&powerRate>0.65,mode:="sea"]

#增加辅机和锅炉功率

otherPower=fread('~/R/AIS_Processing/carbon-main/data/container_aux_boiler_power.csv')
aship=left_join(aship[,sizegroup:=as.integer(sizegroup)],otherPower,'sizegroup')
#查找BaseSFC(g/kwh) Baseline specific fuel consumption 燃油消耗量基准 
# 船舶制造年份分为三段 SFCbase P87
aship=aship[,builtYearGroup:=cut(year_of_built,breaks=c(0,1984,2001,2050),labels=c(1,2,3),right=FALSE)]
baseSFC=fread('~/R/AIS_Processing/carbon-main/data/baseSFC.csv')
#主机要根据发动机载荷进行调整
mainBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup&
                                engineType=='MSD'&                              # 中速柴油机
                                  fuelType=='HFO',                              # Heavy Fuel Oil 重油
                                            baseSFC])
# 主发动机具体油耗 
aship=aship[,mainSFC:=mainBSFC*(0.455*powerRate^2-0.710*powerRate+1.28)]        # 调整 p89
# 辅机和锅炉不需要调整
auxBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup&
                               engineType=='Auxiliary engines'&                 # 辅机
                                 fuelType=='MDO',                               # Marine Diesel Oil 船用柴油
                                           baseSFC])

boilerBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup& 
                                  engineType=='Steam Turbines (and boilers)'&   # 锅炉
                                    fuelType=='HFO',                            # Heavy Fuel Oil 重油
                                              baseSFC])
aship=aship[,auxSFC:=auxBSFC][,boilerSFC:=boilerBSFC]                           # 不调整
# fuel based emission factors 燃料基准排放因子
aship=aship[,mainCO2EF:=3.114][,auxCO2EF:=3.206][,boilerCO2EF:=3.206]#4th IMO GHG table21 p92,单位 g CO2/g fuel

# Global average fuel sulfur content in percentage per year 2.6 for HFO
aship=aship[,mainSO2EF:=2*0.97753*2.6][,auxSO2EF:=2*0.97753*2.6][,boilerSO2EF:=2*0.97753*2.6]#4th IMO GHG table 22

aship=aship[order(time)][,timespan:=shift(time,n=-1)-time]#时间单位为秒
# 主机总排放
aship=aship[,mainCO2EM:=aPower*mainSFC*mainCO2EF*timespan/3600]#单位为克
aship=aship[,mainSO2EM:=aPower*mainSFC*mainSO2EF*timespan/3600]
# 辅机总排放
aship=aship[mode%in%c('berth'),auxCO2EM:=auxBerthKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),auxCO2EM:=auxAnchorKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),auxCO2EM:=auxManKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),auxCO2EM:=auxSeaKW*auxSFC*auxCO2EF*timespan/3600]
# 
aship=aship[mode%in%c('berth'),boilerCO2EM:=boilerBerthKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),boilerCO2EM:=boilerAnchorKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),boilerCO2EM:=boilerManKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),boilerCO2EM:=boilerSeaKW*boilerSFC*boilerCO2EF*timespan/3600]

#SO2排放
aship=aship[mode%in%c('berth'),auxSO2EM:=auxBerthKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),auxSO2EM:=auxAnchorKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),auxSO2EM:=auxManKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),auxSO2EM:=auxSeaKW*auxSFC*auxSO2EF*timespan/3600]

aship=aship[mode%in%c('berth'),boilerSO2EM:=boilerBerthKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),boilerSO2EM:=boilerAnchorKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),boilerSO2EM:=boilerManKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),boilerSO2EM:=boilerSeaKW*boilerSFC*boilerSO2EF*timespan/3600]
aship[!is.na(timespan),list(mCO2=sum(mainCO2EM),auxCO2=sum(auxCO2EM),boilerCO2=sum(boilerCO2EM),mSO2=sum(mainSO2EM),auxSO2=sum(auxSO2EM),boilerSO2=sum(boilerSO2EM)),list(mode)]


temp <- matrix(nrow=0,ncol=2)
temp=data.table(temp)
split <- split(aship,aship$month)
# for (n in 1:6) {
#   temp[V1]=sum((split[[n]])$mainCO2EM)
# }

for (n in 1:12) {
  print(sum((split[[n]])$mainCO2EM))
}

# mmsistemp19=data.table(dir('~/AIS/2019_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi
# mmsistemp20=data.table(dir('~/AIS/2020_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi
# a=data.table(setDT(mmsistemp19), setDT(mmsistemp20), fill=TRUE)
# a=bind_cols(list(mmsistemp19,mmsistemp20))
#   cj_th1 <- left_join(mmsis2019,mmsis2020)
