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
nb_ships=fread('/Users/mizexin/AIS/Ningbo/ningbo_50000ship_IHS.csv')
ship1=fread('/Users/mizexin/AIS/Ningbo/dyn_sta_ship_match.csv')%>%clean_names()
#temp=nb_ships[,.N,shipAndCargType]
nb_ships=clean_names(nb_ships)
nb_ships=nb_ships[!(is.na(gt)&is.na(deadweight))][,c('ais_name','call_sign','ship_size',
                                                     'n','aux_engine_total_kw','aux_engine_stroke_type',
                                                     'tanks'):=NULL]
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

type_match=fread('/Users/mizexin/AIS/Ningbo/nb_ship_type_match.csv')
type_match=clean_names(type_match)

nb_ships=left_join(nb_ships,type_match[,list(ship_type,imo_ship_type=imo)],by='ship_type')

#---加入主机发动机类型，基于rpm---

nb_ships=nb_ships[,main_eng_type:=fcase(engines_rpm<=300,'SSD',engines_rpm%between%c(301,900),'MSD',engines_rpm>=900,'HSD')]


#---加入主机燃油类型--与排放因子对应？-米泽欣再确定下代码---静态参数里缺少propulsion type 跳过

# NA换成empty
nb_ships[nb_ships==""] <- NA
nb_ships$fuel_type_1[is.na(nb_ships$fuel_type_1)]<-'empty'
nb_ships$fuel_type_2[is.na(nb_ships$fuel_type_2)]<-'empty'

# Allocation algorithm for the main engine fuel type GHG4 Table 9
# HFO
nb_ships[fuel_type_1 == 'Residual Fuel' | fuel_type_2 == 'Residual Fuel',main_fuel_type:='HFO']
# nb_ships[propulsion_type == 'Steam Turbine(s), Geared Drive' | ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
# MDO
nb_ships[fuel_type_1 == 'Distillate Fuel' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
nb_ships[fuel_type_1 == 'Distillate Fuel' & fuel_type_2 == 'empty',main_fuel_type:='MDO']
nb_ships[fuel_type_1 ==  'empty' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
nb_ships[fuel_type_1 ==  'Coal' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
nb_ships[fuel_type_1 ==  'Methanol' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='Methanol']
# LNG
# nb_ships[propulsion_type == 'Steam Turbine(s), Geared Drive' | ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
# nb_ships[fuel_type_1 ==  'Residual Fuel' & propulsion_type =='Steam Turbine(s), Geared Drive' & ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
# nb_ships[fuel_type_2 ==  'Residual Fuel' & propulsion_type =='Steam Turbine(s), Geared Drive' & ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
nb_ships[fuel_type_1 ==  'Gas boil-off' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='LNG']
nb_ships[fuel_type_1 ==  'Lng' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='LNG']
nb_ships[fuel_type_1 ==  'Lng' & fuel_type_2 == 'empty',main_fuel_type:='LNG']
nb_ships[fuel_type_1 ==  'empty' & fuel_type_2 == 'Lng',main_fuel_type:='LNG']
nb_ships[fuel_type_2 ==  'Gas boil-off' ,main_fuel_type:='LNG']
# Nuclear
nb_ships[fuel_type_1 ==  'Nuclear' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='Nuclear']
nb_ships[fuel_type_1 ==  'Nuclear' & fuel_type_2 == 'empty',main_fuel_type:='Nuclear']
# Coal
nb_ships[fuel_type_1 ==  'Coal' & fuel_type_2 == 'empty',main_fuel_type:='Coal']
# Methanol
nb_ships[fuel_type_1 ==  'Methanol' ,main_fuel_type:='Methanol']


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

#---异常值----
outlier_value=nb_ships[0,]#设置表头
#year
hist(nb_ships$year,
     xlab = "year",
     main = "Histogram of year",
     breaks = sqrt(nrow(nb_ships))) 
outlier_year = nb_ships[year %in% c('2021','2022')]
outlier_value = rbind(outlier_value,outlier_year)
#breadth
hist(nb_ships$breadth,
     xlab = "breadth",
     main = "Histogram of breadth",
     breaks = sqrt(nrow(nb_ships))) 
outlier_breadth = nb_ships[breadth == 0]
outlier_value = rbind(outlier_value,outlier_breadth)
#剔除异常值（63行）
nb_ships = setdiff(nb_ships,outlier_value )
# fwrite(nb_ships,'C:\\Users\\liuxianchao\\Desktop\\nb_ships.csv')

#---------------无异常--------------------
#deadweight 
hist(nb_ships$deadweight,
     xlab = "deadweight",
     main = "Histogram of deadweight",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#draught 
hist(nb_ships$draught,
     xlab = "draught",
     main = "Histogram of draught",
     breaks = sqrt(nrow(nb_ships))) 
#outlier_draught = nb_ships[draught == 0]
#outlier_value = rbind(outlier_value,outlier_draught)#无异常值
#engine_stroke_type
hist(nb_ships$engine_stroke_type,
     xlab = "engine_stroke_type",
     main = "Histogram of engine_stroke_type",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#engines_rpm 
hist(nb_ships$engines_rpm ,
     xlab = "engines_rpm ",
     main = "Histogram of engines_rpm ",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#gt
hist(nb_ships$gt ,
     xlab = "gt ",
     main = "Histogram of gt ",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#service_speed
hist(nb_ships$service_speed ,
     xlab = "service_speed ",
     main = "Histogram of service_speed ",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#teu 
hist(nb_ships$teu ,
     xlab = "teu ",
     main = "Histogram of teu ",
     breaks = sqrt(nrow(nb_ships))) #无异常值
#total_kw_main_eng
hist(nb_ships$total_kw_main_eng ,
     xlab = "total_kw_main_eng ",
     main = "Histogram of total_kw_main_eng ",
     breaks = sqrt(nrow(nb_ships))) #无异常值

data=data.frame(nb_ships[,c('year','breadth','deadweight','draught','engine_stroke_type','engines_rpm','gt','length','service_speed','teu','total_kw_main_eng')])
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_histogram(alpha=0.6,bins=153329) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(~text)
p


#---------------排放清单--------------
#
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
#-------------------------------------------
# ship1=fread('C:\\Users\\liuxianchao\\Desktop\\dyn_sta_ship_match.csv')%>%clean_names()

ship1=ship1[,c('mmsi','speed','npoints','hours','distnm','ship_type_code')]
setkey(ship1,mmsi)

ship_stasic=nb_ships

ship1=left_join(ship1,ship_stasic,by='mmsi')
ship1=na.omit(ship1)

full_ships0 = ship1
AE_AB_Power=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')%>%clean_names()
setnames(full_ships0,c('ship_type','imo_ship_type','engine_stroke_type'),
         c('ship_type1','ship_type','stroke'))
#test=left_join(full_ships0,AE_AB_Power,by='ship_type')
#test=AE_AB_Power[,.N,list(ship_type=ship_type)][full_ships0,on=.(ship_type),nomatch=NULL]
test0=full_ships0[,loadFactor:=round(((speed/10)/(service_speed/0.94))^3,2)/0.867/0.917][loadFactor>1,loadFactor:=1][,load_bin:=round(loadFactor*100)][load_bin>20,load_bin:=20][load_bin<2,load_bin:=2]

test0=test0[,ae_eng_type:='Auxiliary_Engines'][,ab_eng_type:='Steam_Boiler']
test0=test0[,ae_fuel_type:='MDO'][,ab_fuel_type:='MDO']

test0=test0[,gen:=fcase(year <1984,1,
                        year%between%c(1984,2000),2,
                        year>=2001,3)]
test0=test0[,tier:=fcase(year<=2000,0,
                         year%between%c(2001,2010),1,
                         year%between%c(2011,2015),2,
                         year>=2016,3)]
test0=test0[,mode:=fcase(speed>=0&speed<10,'berth',
                         speed>=10&speed<30,'anchor',
                         speed>=30&loadFactor<=0.2,'manoeuvring',
                         speed>=30&loadFactor>=0.2&loadFactor<0.65,'sea',
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

AE_AB_Power=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_17_Auxiliary_engine_boiler_power_output.csv')
test0=test0[,MEPower:=loadFactor*total_kw_main_eng]
#注意船舶类型字段可能需要修改
test0=AE_AB_Power[,list(ship_type=ShipType,type_bin,size_bin,mode,ABPower,AEPower)][test0,on=.(ship_type,size_bin,mode)]


#燃油消耗
baseSFC=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_19_baseSFC.csv')

test0=baseSFC[,list(main_eng_type=EngineType,main_fuel_type=FuelType,gen,ME_SFCbase=SFCbase)][test0,on=.(main_eng_type,main_fuel_type,gen)]
test0=baseSFC[,list(ae_eng_type=EngineType,ae_fuel_type=FuelType,gen,AE_SFCbase=SFCbase)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
test0=baseSFC[,list(ab_eng_type=EngineType,ab_fuel_type=FuelType,gen,AB_SFCbase=SFCbase)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
test0=test0[,ME_SFC:=ME_SFCbase*(0.445*loadFactor^2-0.710*loadFactor+1.280)][,ME_FC:=ME_SFC*loadFactor*total_kw_main_eng*hours]
test0=test0[loadFactor<0.07,ME_FC:=0] # IMO4表示小于0.07就不消耗燃油 
test0=test0[,AE_FC:=AE_SFCbase*AEPower*hours][,AB_FC:=AB_SFCbase*ABPower*hours]


#二氧化碳
CO2table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_45_CO2_EF.csv')
LLAtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO_LowLoadAdjustmentFactors.csv')
test0=LLAtable[test0,on='load_bin']
#test0=test0[,ME_fuelType:='MDO'][,AE_fuelType:='MDO'][,AB_fuelType:='MDO']
test0=CO2table[,list(main_fuel_type=FuelType,ME_CO2EF=CO2EF)][test0,on='main_fuel_type']#left_join
test0=CO2table[,list(ae_fuel_type=FuelType,AE_CO2EF=CO2EF)][test0,on='ae_fuel_type']
test0=CO2table[,list(ab_fuel_type=FuelType,AB_CO2EF=CO2EF)][test0,on='ab_fuel_type']
test0=test0[,ME_CO2:=ME_FC*ME_CO2EF*CO2_LLA][,AE_CO2:=AE_FC*AE_CO2EF][,AB_CO2:=AB_FC*AB_CO2EF]

#硫化物
SOxtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_47_SOx_EF.csv')
test0=SOxtable[,list(main_fuel_type=FuelType,ME_SOxEF=SOxEF)][test0,on='main_fuel_type']#left_join
test0=SOxtable[,list(ae_fuel_type=FuelType,AE_SOxEF=SOxEF)][test0,on='ae_fuel_type']
test0=SOxtable[,list(ab_fuel_type=FuelType,AB_SOxEF=SOxEF)][test0,on='ab_fuel_type']
test0=test0[,ME_SOx:=ME_FC*ME_SOxEF*SOx_LLA][,AE_SOx:=AE_FC*AE_SOxEF][,AB_SOx:=AB_FC*AB_SOxEF]

#黑炭

test0=test0[,BC_load_bin:=floor(loadFactor*10)+2][loadFactor<0.05,BC_load_bin:=1][loadFactor>=1,BC_load_bin:=11]
#test0=test0[,stroke:=2]#假设二冲程
ME_BCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_63_BC_EF_forME.csv') %>% distinct()#重复行导致报错
AE_AB_BCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_64_BC_EF_forAE.csv')
test0=ME_BCtable[,list(BC_load_bin,main_eng_type=EngineType,main_fuel_type=FuelType,stroke,ME_BC_EFf)][test0,on=.(BC_load_bin,main_eng_type,main_fuel_type,stroke)]
test0=test0[,ME_BC:=ME_FC*ME_BC_EFf*hours]
#IMO4中没有副机MDO等油耗的BC排放，covered by table 52 不明白。目前假设与锅炉的相同
test0=AE_AB_BCtable[,list(ae_eng_type=EngineType,ae_fuel_type=FuelType,AE_BC_EFe=AE_AB_BC_EFe)][test0,on=.(ae_eng_type,ae_fuel_type)]
test0=AE_AB_BCtable[,list(ab_eng_type=EngineType,ab_fuel_type=FuelType,AB_BC_EFe=AE_AB_BC_EFe)][test0,on=.(ab_eng_type,ab_fuel_type)]
test0=test0[,AE_BC:=AEPower*AE_BC_EFe*hours][,AB_BC:=ABPower*AB_BC_EFe*hours]

#氮化物
NOxtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_50_NOx_EF.csv')
test0=NOxtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,tier,ME_NOxEF=NOxEF)][test0,on=.(main_fuel_type,main_eng_type,tier)]#left_join
test0=NOxtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,tier,AE_NOxEF=NOxEF)][test0,on=.(ae_eng_type,ae_fuel_type,tier)]
test0=NOxtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,tier,AB_NOxEF=NOxEF)][test0,on=.(ab_fuel_type,ab_eng_type,tier)]
test0=test0[,ME_NOx:=MEPower*ME_NOxEF*NOx_LLA*hours][,AE_NOx:=AEPower*AE_NOxEF*hours][,AB_NOx:=ABPower*AB_NOxEF*hours]

#甲烷

CH4table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_Appendix_B_table_6_CH4_EF.csv')
test0=CH4table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_CH4EF=CH4EF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
test0=CH4table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_CH4EF=CH4EF)][test0,on=.(ae_eng_type,ae_fuel_type)]
test0=CH4table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_CH4EF=CH4EF)][test0,on=.(ab_eng_type,ab_fuel_type)]
test0=test0[,ME_CH4:=MEPower*ME_CH4EF*CH4_LLA*hours][,AE_CH4:=AEPower*AE_CH4EF*hours][,AB_CH4:=ABPower*AB_CH4EF*hours]


#一氧化碳

COtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_57_CO_EF.csv')
test0=COtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_COEF=COEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
test0=COtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_COEF=COEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
test0=COtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_COEF=COEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
test0=test0[,ME_CO:=MEPower*ME_COEF*CO_LLA*hours][,AE_CO:=AEPower*AE_COEF*hours][,AB_CO:=ABPower*AB_COEF*hours]

#一氧化二氮

N2Otable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_59_N2O_EF.csv')
test0=N2Otable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_N2OEF=N2OEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
test0=N2Otable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_N2OEF=N2OEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
test0=N2Otable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_N2OEF=N2OEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
test0=test0[,ME_N2O:=MEPower*ME_N2OEF*N2O_LLA*hours][,AE_N2O:=AEPower*AE_N2OEF*hours][,AB_N2O:=ABPower*AB_N2OEF*hours]

#颗粒物PM10

PM10table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM10_EF.csv')
test0=PM10table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,gen,ME_PM10EF=PM10EF)][test0,on=.(main_eng_type,main_fuel_type,gen)]#left_join
test0=PM10table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,gen,AE_PM10EF=PM10EF)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
test0=PM10table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,gen,AB_PM10EF=PM10EF)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
test0=test0[,ME_PM10:=MEPower*ME_PM10EF*PM10_LLA*hours][,AE_PM10:=AEPower*AE_PM10EF*hours][,AB_PM10:=ABPower*AB_PM10EF*hours]

#颗粒物PM2.5

PM2.5table=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_52_PM2.5_EF.csv')
test0=PM2.5table[,list(main_fuel_type=FuelType,main_eng_type=EngineType,gen,ME_PM2.5EF=PM2.5EF)][test0,on=.(main_eng_type,main_fuel_type,gen)]#left_join
test0=PM2.5table[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,gen,AE_PM2.5EF=PM2.5EF)][test0,on=.(ae_eng_type,ae_fuel_type,gen)]
test0=PM2.5table[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,gen,AB_PM2.5EF=PM2.5EF)][test0,on=.(ab_eng_type,ab_fuel_type,gen)]
test0=test0[,ME_PM2.5:=MEPower*ME_PM2.5EF*PM2.5_LLA*hours][,AE_PM2.5:=AEPower*AE_PM2.5EF*hours][,AB_PM2.5:=ABPower*AB_PM2.5EF*hours]


#非甲烷挥发性有机化合物NMVOC

NMVOCtable=fread('~/R/AIS_Processing/carbon-main/data/IMO4_table/IMO4_table_61_NMVOC_EF.csv')
test0=NMVOCtable[,list(main_fuel_type=FuelType,main_eng_type=EngineType,ME_NMVOCEF=NMVOCEF)][test0,on=.(main_eng_type,main_fuel_type)]#left_join
test0=NMVOCtable[,list(ae_fuel_type=FuelType,ae_eng_type=EngineType,AE_NMVOCEF=NMVOCEF)][test0,on=.(ae_eng_type,ae_fuel_type)]
test0=NMVOCtable[,list(ab_fuel_type=FuelType,ab_eng_type=EngineType,AB_NMVOCEF=NMVOCEF)][test0,on=.(ab_eng_type,ab_fuel_type)]
test0=test0[,ME_NMVOC:=MEPower*ME_NMVOCEF*NMVOC_LLA*hours][,AE_NMVOC:=AEPower*AE_NMVOCEF*hours][,AB_NMVOC:=ABPower*AB_NMVOCEF*hours]

aa = test0$mmsi%>%unique()
