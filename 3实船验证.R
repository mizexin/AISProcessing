library(data.table)
library(plyr)
library(dplyr)
ship_report_2019=fread('/Users/mizexin/论文数据/china_2019_container_fuel_report.csv',
  select =c("IMO Number","Deadweight tonnage","Distance travelled","Hours underway","Quantity"))%>%
  rename(.,'imo'='IMO Number','deadweight'='Deadweight tonnage','dist'='Distance travelled','hours'='Hours underway','quantity'='Quantity')

ship_result=fread('/Volumes/Samsung\ SSD/汇总结果/fix/2019.csv',
                  select =c("imo","Total_FC","distnm","durhour"))%>%na.omit(.)
ship_result$Total_FC=ship_result$Total_FC/1000000
quantity = aggregate(quantity ~ imo, data = ship_report_2019,sum)

quantity = quantity[order(quantity$imo),]

report_2019 = ship_report_2019[,.(imo,deadweight,dist,hours)]%>%unique(.,by = 'imo')

report_2019 = cbind(report_2019[order(report_2019$imo),],quantity[,'quantity'])%>%
  rename(.,'quantity'='V2')
a = ship_result[report_2019,on=.(imo)]%>%na.omit(.)

a=a[distnm>=9000][quantity>=1500][hours>=1000][dist>=10000]

a=a[,size_bin:=fcase(deadweight%between%c(0,999),1,
                               deadweight%between%c(1000,1999),2,
                               deadweight%between%c(2000,2999),3,
                               deadweight%between%c(3000,4999),4,
                               deadweight%between%c(5000,7999),5,
                               deadweight%between%c(8000,11999),6,
                               deadweight%between%c(12000,14499),7,
                               deadweight%between%c(14500,19999),8,
                               deadweight>=20000,9)]
a = rename(a,'FC_count'='Total_FC','dist_count'='distnm','hours_count'='gap_hours','FC'='quantity')
a=a[,dif_FC:=(FC-FC_count)/FC][,dif_dist:=(dist-dist_count)/dist][,dif_hours:=(hours-hours_count)/hours]%>%round(.,2)
fwrite(a,'/Users/mizexin/论文数据/2019实船验证对比.csv',sep = ',')                    




library(ggplot2)
aship=fread("/Users/mizexin/AIS/2019_year_container/219300000.csv",sep = ',')
qplot(aship$lon,aship$lat)

