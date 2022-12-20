# 载入相关包
library(RODBC)
library(foreach)
library(data.table)
library(lubridate)
library('dplyr')
library('stringr')
library('ggplot2')
library('ggthemes')
library(ggpubr)
library(sqldf)
library(explore)
library(DataExplorer)
library(janitor)
library(skimr)
library(readxl)

Sys.setlocale("LC_ALL","Chinese") 

#读取宁波数据
nb_ships=fread('D:/Git/Rprojects/ningbo/ningbo/data/ningbo_ships_2020/ningbo_50000ship_IHS.csv')
#temp=nb_ships[,.N,shipAndCargType]
nb_ships=clean_names(nb_ships)
nb_ships=nb_ships[!(is.na(gt)&is.na(deadweight))][,c('ais_name','call_sign','ship_size','n','aux_engine_total_kw','aux_engine_stroke_type','tanks'):=NULL]

nb_ships[is.na(total_kw_main_eng)]

nb_ships[,v27:=NULL]
#--------补充总吨,用相似船舶length推测-------
null_gt=nb_ships[is.na(gt)|gt==0]
for(i in seq(1,nrow(null_gt))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_gt[i]$mmsi
  alength=null_gt[i]$length
  type=null_gt[i]$ship_type 
  similarships=nb_ships[!(is.na(gt)|gt==0)][ship_type==type][length%between%c(alength*0.8,alength*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(gt)|gt==0)][length%between%c(alength*0.8,alength*1.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(alength-length))][1]$gt)#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,gt:=value]
}


#--------补充最大载重吨,用相似船舶gt推测-------
null_dwt=nb_ships[is.na(deadweight)|deadweight==0]
for(i in seq(1,nrow(null_dwt))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_dwt[i]$mmsi
  gton=null_dwt[i]$gt
  type=null_dwt[i]$ship_type 
  similarships=nb_ships[!(is.na(deadweight)|deadweight==0)][ship_type==type][gt%between%c(gton*0.8,gton*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(deadweight)|deadweight==0)][gt%between%c(gton*0.8,gton*1.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(gton-gt))][1]$deadweight)#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,deadweight:=value]
}

#--------补充主机引擎,用dwt-------
null_kw=nb_ships[is.na(total_kw_main_eng)|total_kw_main_eng==0]
for(i in seq(1,nrow(null_kw))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_kw[i]$mmsi
  adwt=null_kw[i]$deadweight
  type=null_kw[i]$ship_type
  similarships=nb_ships[!(is.na(total_kw_main_eng)|total_kw_main_eng==0)][ship_type==type][deadweight%between%c(adwt*0.8,adwt*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(total_kw_main_eng)|total_kw_main_eng==0)][deadweight%between%c(adwt*0.8,adwt*1.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(adwt-deadweight))][1]$total_kw_main_eng)#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,total_kw_main_eng:=value]
}

#--------补充服务航速,用主机功率-------
null_speed=nb_ships[is.na(service_speed)|service_speed==0]
for(i in seq(1,nrow(null_speed))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_speed[i]$mmsi
  aeng=null_speed[i]$total_kw_main_eng
  type=null_speed[i]$ship_type
  similarships=nb_ships[!(is.na(service_speed)|service_speed==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.5)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(service_speed)|service_speed==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.5)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$service_speed#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,service_speed:=value]
}

#--------补充主机转速,用主机功率-------
null_rpm=nb_ships[is.na(engines_rpm)|engines_rpm==0]
for(i in seq(1,nrow(null_rpm))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_rpm[i]$mmsi
  aeng=null_rpm[i]$total_kw_main_eng
  type=null_rpm[i]$ship_type
  similarships=nb_ships[!(is.na(engines_rpm)|engines_rpm==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(engines_rpm)|engines_rpm==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$engines_rpm#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,engines_rpm:=value]
}

#--------补充主机冲程,用主机功率-------
null_stroke=nb_ships[is.na(engine_stroke_type)|engine_stroke_type==0]
for(i in seq(1,nrow(null_stroke))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_stroke[i]$mmsi
  aeng=null_stroke[i]$total_kw_main_eng
  type=null_stroke[i]$ship_type
  similarships=nb_ships[!(is.na(engine_stroke_type)|engine_stroke_type==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(engine_stroke_type)|engine_stroke_type==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$engine_stroke_type#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,engine_stroke_type:=value]
}

#---加入船舶类型，根据匹配表---

type_match=fread('D:/Git/Rprojects/ningbo/ningbo/data/nb_ship_type_match.csv')
type_match=clean_names(type_match)

nb_ships=left_join(nb_ships,type_match[,list(ship_type,imo_ship_type=imo)],by='ship_type')

#---加入主机发动机类型，基于rpm---

nb_ships=nb_ships[,main_eng_type:=fcase(engines_rpm<=300,'SSD',engines_rpm%between%c(301,900),'MSD',engines_rpm>=900,'HSD')]


#---加入主机燃油类型--与排放因子对应？-米泽欣再确定下代码---

nb_ships = nb_ships[,main_fuel_type:=fcase(fuel_type_1=='Residual Fuel'|fuel_type_2=='Residual Fuel','HFO',
                                           fuel_type_1=='Distillate Fuel' & fuel_type_2=='Distillate Fuel','MDO',
                                           fuel_type_1=='Methanol' & fuel_type_2=='Distillate Fuel','MDO',
                                           fuel_type_1=='Gas Boil Off' & fuel_type_2=='Distillate Fuel','LNG',
                                           fuel_type_1=='Lng' & fuel_type_2=='Distillate Fuel','LNG',
                                           fuel_type_1=='Lpg' & fuel_type_2=='Distillate Fuel','LNG')]


#---补充主机燃油类型,基于gt----

null_fuel=nb_ships[is.na(main_fuel_type)|main_fuel_type==0]
for(i in seq(1,nrow(null_fuel))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_fuel[i]$mmsi
  agt=null_fuel[i]$gt
  type=null_fuel[i]$ship_type
  similarships=nb_ships[!(is.na(main_fuel_type)|main_fuel_type==0)][ship_type==type][gt%between%c(agt*0.8,agt*2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=nb_ships[!(is.na(main_fuel_type)|main_fuel_type==0)][gt%between%c(agt*0.8,agt*2)]#不需要限制船舶类型
  }
  value=(similarships[which.min(abs(agt-gt))][1]$main_fuel_type)#如果有多个值，取第一个
  nb_ships=nb_ships[mmsi==ammsi,main_fuel_type:=value]
}


skim(nb_ships)