
# -----------对比初期步骤---------------
library(dplyr)
library(data.table)
mmsis1=data.table(data.table(dir('~/AIS/2019_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi)

mmsis2=data.table(data.table(dir('~/AIS/2020_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi)

ships=rbind(mmsis1,mmsis2)
colnames(ships)='MMSI'
rm(mmsis1,mmsis2)
ships = unique(ships)
ships$MMSI=as.integer(ships$MMSI)
# fwrite(ships,'/Users/mizexin/Desktop/集装箱船的MMSI.csv')
# IHS = fread('/Users/mizexin/Desktop/论文数据/原始文档/IHS3504.csv')
IHS = fread('/Users/mizexin/Desktop/论文数据/原始文档/ShipsResultsnew.csv')
Hifleet=fread("/Users/mizexin/Desktop/论文数据/原始文档/hifleetships_brief_container_for_student.csv")
Hifleet=na.omit(Hifleet)
# 集装箱数据 不在IHS的部分 1853
a = ships[(!ships$MMSI %in% IHS[,.N,MMSI]$MMSI)]
# 不在IHS的部分 但在Hifleet的部分 提取imo 960
hifleet963imo=data.table(Hifleet[mmsi%in%a[,.N,MMSI]$MMSI]$imo)
hifleet963mmsi=data.table(Hifleet[mmsi%in%a[,.N,MMSI]$MMSI]$mmsi)
refindfile = cbind(hifleet963mmsi,hifleet963imo)
names(refindfile) = c('mmsi','imo')
fwrite(refindfile,'/Users/mizexin/Desktop/refindfile).csv')

findout=data.table(Hifleet[mmsi%in%ships[,.N,MMSI]$MMSI]$imo)



hifout = data.table(Hifleet[(!mmsi%in%a[,.N,MMSI]$MMSI)]$imo)
# 提取960的mmsi
hifleet1800v2=data.table(Hifleet[mmsi%in%a[,.N,MMSI]$MMSI]$mmsi)
# fwrite(hifleet1800v2,'/Users/mizexin/Desktop/mmsi22.csv')

# 写出885条船
hfinal = fread('/Users/mizexin/Desktop/论文数据/hifleetships_brief_container_for_student.csv')
final = data.table(hfinal[MMSI%in%b$V1])
fwrite(final,'/Users/mizexin/Desktop/final.csv')

# 
library(dplyr)
library(data.table)
mmsis1=data.table(data.table(dir('~/AIS/2019_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi)
mmsis2=data.table(data.table(dir('~/AIS/2020_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi)
ships=rbind(mmsis1,mmsis2)
colnames(ships)='MMSI'
rm(mmsis1,mmsis2)
ships = unique(ships)
ships$MMSI=as.integer(ships$MMSI)
# fwrite(ships,'/Users/mizexin/Desktop/所有集装箱船的MMSI.csv')
IHS = fread('/Users/mizexin/Desktop/论文数据/原始文档/ShipResults.csv')
Hifleet=fread("/Users/mizexin/R/AIS_Processing/carbon-main/data/hifleetships_dupl.csv")%>%unique()
# 集装箱数据 不在IHS的部分 1845
a = ships[(!ships$MMSI %in% IHS[,.N,MMSI]$MMSI)]

# 不在IHS的部分 但在Hifleet的部分 提取imo 1845
hifleet1845imo=data.table(Hifleet[mmsi%in%a[,.N,MMSI]$MMSI]$imo)

fwrite(hifleet1845imo,'/Users/mizexin/Desktop/hifleet1845imo.csv')
hifleet1851mmsi=data.table(Hifleet[mmsi%in%a[,.N,MMSI]$MMSI]$mmsi)

#---------------插补步骤---------------

library(janitor)
library(data.table)
library(dplyr)
ships_static_data=fread('/Users/mizexin/Desktop/论文数据/ShipsResultsnew.csv',nThread = 10)%>%clean_names(.)


#--------补充总吨,用相似船舶length推测-------
null_gt=ships_static_data[is.na(gt)|gt==0]
for(i in seq(1,nrow(null_gt))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_gt[i]$mmsi
  alength=null_gt[i]$length
  type=null_gt[i]$ship_type 
  similarships=ships_static_data[!(is.na(gt)|gt==0)][ship_type==type][length%between%c(alength*0.8,alength*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(gt)|gt==0)][length%between%c(alength*0.8,alength*1.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(alength-length))][1]$gt)#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,gt:=value]
}

#--------补充最大载重吨,用相似船舶gt推测 有改动 船舶系数范围------ 
null_dwt=ships_static_data[is.na(deadweight)|deadweight==0]
for(i in seq(1,nrow(null_dwt))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_dwt[i]$mmsi
  gton=null_dwt[i]$gt
  type=null_dwt[i]$ship_type 
  similarships=ships_static_data[!(is.na(deadweight)|deadweight==0)][ship_type==type][gt%between%c(gton*0.8,gton*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(deadweight)|deadweight==0)][gt%between%c(gton*0.8,gton*2.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(gton-gt))][1]$deadweight)#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,deadweight:=value]
}


#--------补充主机引擎,用dwt-------
null_kw=ships_static_data[is.na(total_kw_main_eng)|total_kw_main_eng==0]
for(i in seq(1,nrow(null_kw))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_kw[i]$mmsi
  adwt=null_kw[i]$deadweight
  type=null_kw[i]$ship_type
  similarships=ships_static_data[!(is.na(total_kw_main_eng)|total_kw_main_eng==0)][ship_type==type][deadweight%between%c(adwt*0.8,adwt*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(total_kw_main_eng)|total_kw_main_eng==0)][deadweight%between%c(adwt*0.8,adwt*1.2)]#不需要限制船舶类型
  }
  value=round(similarships[which.min(abs(adwt-deadweight))][1]$total_kw_main_eng)#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,total_kw_main_eng:=value]
}

#--------补充服务航速,用主机功率-------
null_speed=ships_static_data[is.na(service_speed)|service_speed==0]
for(i in seq(1,nrow(null_speed))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_speed[i]$mmsi
  aeng=null_speed[i]$total_kw_main_eng
  type=null_speed[i]$ship_type
  similarships=ships_static_data[!(is.na(service_speed)|service_speed==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.5)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(service_speed)|service_speed==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.5)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$service_speed#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,service_speed:=value]
}

#--------补充主机转速,用主机功率-------
null_rpm=ships_static_data[is.na(engines_rpm)|engines_rpm==0]
for(i in seq(1,nrow(null_rpm))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_rpm[i]$mmsi
  aeng=null_rpm[i]$total_kw_main_eng
  type=null_rpm[i]$ship_type
  similarships=ships_static_data[!(is.na(engines_rpm)|engines_rpm==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(engines_rpm)|engines_rpm==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$engines_rpm#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,engines_rpm:=value]
}

#--------补充主机冲程,用主机功率-------
null_stroke=ships_static_data[is.na(engine_stroke_type)|engine_stroke_type==0]
for(i in seq(1,nrow(null_stroke))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_stroke[i]$mmsi
  aeng=null_stroke[i]$total_kw_main_eng
  type=null_stroke[i]$ship_type
  similarships=ships_static_data[!(is.na(engine_stroke_type)|engine_stroke_type==0)][ship_type==type][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(engine_stroke_type)|engine_stroke_type==0)][total_kw_main_eng%between%c(aeng*0.8,aeng*1.2)]#不需要限制船舶类型
  }
  value=similarships[which.min(abs(aeng-total_kw_main_eng))][1]$engine_stroke_type#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,engine_stroke_type:=value]
}

#--------主机类型--------
# Engine Type
ships_static_data[grep(pattern = "Oil",propulsion_type) & engines_rpm <= 300, enginetype:='SSD']
ships_static_data[grep(pattern = "Oil",propulsion_type) & engines_rpm > 300 & engines_rpm <= 900, enginetype:='MSD']
ships_static_data[grep(pattern = "Oil",propulsion_type) & engines_rpm > 900 ,enginetype:='HSD']

# NA换成empty
ships_static_data[ships_static_data==""] <- NA
ships_static_data$fuel_type_1[is.na(ships_static_data$fuel_type_1)]<-'empty'
ships_static_data$fuel_type_2[is.na(ships_static_data$fuel_type_2)]<-'empty'

# Allocation algorithm for the main engine fuel type GHG4 Table 9
# HFO
ships_static_data[fuel_type_1 == 'Residual Fuel' | fuel_type_2 == 'Residual Fuel',main_fuel_type:='HFO']
ships_static_data[propulsion_type == 'Steam Turbine(s), Geared Drive' | ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
# MDO
ships_static_data[fuel_type_1 == 'Distillate Fuel' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
ships_static_data[fuel_type_1 == 'Distillate Fuel' & fuel_type_2 == 'empty',main_fuel_type:='MDO']
ships_static_data[fuel_type_1 ==  'empty' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
ships_static_data[fuel_type_1 ==  'Coal' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='MDO']
ships_static_data[fuel_type_1 ==  'Methanol' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='Methanol']
# LNG
ships_static_data[propulsion_type == 'Steam Turbine(s), Geared Drive' | ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
ships_static_data[fuel_type_1 ==  'Residual Fuel' & propulsion_type =='Steam Turbine(s), Geared Drive' & ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
ships_static_data[fuel_type_2 ==  'Residual Fuel' & propulsion_type =='Steam Turbine(s), Geared Drive' & ship_type == 'Liquefied gas tanker',main_fuel_type:='LNG']
ships_static_data[fuel_type_1 ==  'Gas boil-off' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='LNG']
ships_static_data[fuel_type_1 ==  'Lng' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='LNG']
ships_static_data[fuel_type_1 ==  'Lng' & fuel_type_2 == 'empty',main_fuel_type:='LNG']
ships_static_data[fuel_type_1 ==  'empty' & fuel_type_2 == 'Lng',main_fuel_type:='LNG']
ships_static_data[fuel_type_2 ==  'Gas boil-off' ,main_fuel_type:='LNG']
# Nuclear
ships_static_data[fuel_type_1 ==  'Nuclear' & fuel_type_2 == 'Distillate Fuel',main_fuel_type:='Nuclear']
ships_static_data[fuel_type_1 ==  'Nuclear' & fuel_type_2 == 'empty',main_fuel_type:='Nuclear']
# Coal
ships_static_data[fuel_type_1 ==  'Coal' & fuel_type_2 == 'empty',main_fuel_type:='Coal']
# Methanol
ships_static_data[fuel_type_1 ==  'Methanol' ,main_fuel_type:='Methanol']

                        有改动
#---补充主机燃油类型,基于gt----

null_fuel=ships_static_data[is.na(main_fuel_type)|main_fuel_type==0]
for(i in seq(1,nrow(null_fuel))){
  if(i%%10==0){print(i)}#i=1
  ammsi=null_fuel[i]$mmsi
  agt=null_fuel[i]$gt
  type=null_fuel[i]$ship_type
  similarships=ships_static_data[!(is.na(main_fuel_type)|main_fuel_type==0)][ship_type==type][gt%between%c(agt*0.8,agt*2)]
  if(nrow(similarships)<3){ #少于三个相似船舶，建议从其他类型查找相似船舶
    similarships=ships_static_data[!(is.na(main_fuel_type)|main_fuel_type==0)][gt%between%c(agt*0.8,agt*4)]#不需要限制船舶类型
  }
  value=(similarships[which.min(abs(agt-gt))][1]$main_fuel_type)#如果有多个值，取第一个
  ships_static_data=ships_static_data[mmsi==ammsi,main_fuel_type:=value]
}


skim(ships_static_data)



library(patchwork)
#year
year = hist(ships_static_data$year,
     xlab = "year",
     main = "year",
     breaks = sqrt(nrow(ships_static_data))) 
#breadth
breath = hist(ships_static_data$breadth,
     xlab = "breadth",
     main = "breadth",
     breaks = sqrt(nrow(ships_static_data)))
year + breath
#deadweight 
hist(ships_static_data$deadweight,
     xlab = "deadweight",
     main = "deadweight",
     breaks = sqrt(nrow(ships_static_data))) 
#draught 
hist(ships_static_data$draught,
     xlab = "draught",
     main = "draught",
     breaks = sqrt(nrow(ships_static_data))) 

hist(ships_static_data$engine_stroke_type,
     xlab = "engine_stroke_type",
     main = "engine_stroke_type",
     breaks = sqrt(nrow(ships_static_data))) #无异常值
#engines_rpm 
hist(ships_static_data$engines_rpm ,
     xlab = "engines_rpm ",
     main = "engines_rpm ",
     breaks = sqrt(nrow(ships_static_data))) #无异常值
#gt
hist(ships_static_data$gt ,
     xlab = "gt ",
     main = "gt ",
     breaks = sqrt(nrow(ships_static_data))) #无异常值
#service_speed
hist(ships_static_data$service_speed ,
     xlab = "service_speed ",
     main = "service_speed ",
     breaks = sqrt(nrow(ships_static_data))) #无异常值
#teu 
hist(ships_static_data$teu ,
     xlab = "teu ",
     main = "teu ",
     breaks = sqrt(nrow(ships_static_data))) #无异常值
#total_kw_main_eng
hist(ships_static_data$total_kw_main_eng ,
     xlab = "total_kw_main_eng ",
     main = "total_kw_main_eng ",
     breaks = sqrt(nrow(ships_static_data))) #无异常值

data=data.frame(ships_static_data[,c('year','breadth','deadweight','draught','engine_stroke_type','engines_rpm','gt','length','service_speed','teu','total_kw_main_eng')])
  data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),10))

# plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_histogram(alpha=0.6,bins=10) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)
p
