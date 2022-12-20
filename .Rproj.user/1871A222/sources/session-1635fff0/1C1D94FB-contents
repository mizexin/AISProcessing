library(data.table)
library(showtext)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
showtext_auto()

# 读取两年的合并文件   
# sum_2019=fread("~/论文数据/2019.csv",sep = ',')
sum_2019=fread("/Volumes/Samsung\ SSD/汇总结果/fix/2019.csv",sep = ',')
sum_2019 = sum_2019[,year:=2019]

# 新加一列表示年份
# sum_2020=fread("~/论文数据/2020.csv",sep = ',')
sum_2020=fread("/Volumes/Samsung\ SSD/汇总结果/fix/2020.csv",sep = ',')
sum_2020 = sum_2020[,year:=2020]

sum_dt  = rbind(sum_2019,sum_2020)
rm(sum_2019,sum_2020)

# 只保留带有载重量的数据
sum_dt  = sum_dt[complete.cases(sum_dt[,capacity]),]

# 划分大小
sum_dt=sum_dt[,size_bin:=fcase(capacity%between%c(0,999),1,
                                capacity%between%c(1000,1999),2,
                                capacity%between%c(2000,2999),3,
                                capacity%between%c(3000,4999),4,
                                capacity%between%c(5000,7999),5,
                                capacity%between%c(8000,11999),6,
                                capacity%between%c(12000,14499),7,
                                capacity%between%c(14500,19999),8,
                                capacity>=20000,9)]


#  船舶尺寸——二氧化碳
# 单位万吨
CO2_size_bin = aggregate(sum_dt$Total_CO2,by=list(sum_dt$size_bin,sum_dt$year),sum)%>%rename(.,'capacity'= 'Group.1','year'= 'Group.2','CO2' = 'x')
CO2_size_bin$CO2  = CO2_size_bin$CO2/1000000000
CO2_size_bin[,2] = as.character(CO2_size_bin$year)
CO2_size_bin %>%
ggplot(aes(fill=year,x=capacity, y=CO2)) + 
  geom_bar(position="dodge", stat="identity",width=0.7,alpha=0.7) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_fill_manual(values = c("#3498db", "#1abc9c")) +
  ylab(label = 'CO2/万吨') +
  xlab(label = '船舶尺寸')#+ theme_bw()
# 船舶尺寸-航程
# 单位万海里
distnm_size_bin = aggregate(sum_dt$distnm,by=list(sum_dt$size_bin,sum_dt$year),sum)%>%rename(.,'capacity'= 'Group.1','year'= 'Group.2','distnm' = 'x')
distnm_size_bin[,2] = as.character(distnm_size_bin$year)
distnm_size_bin$distnm  = distnm_size_bin$distnm/10000
distnm_size_bin %>%
  ggplot(aes(fill=year,x=capacity, y=distnm)) + 
  geom_bar(position="dodge", stat="identity",width=0.7,alpha=0.7) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_fill_manual(values = c("#3498db", "#1abc9c")) +
  ylab(label = '航程/万海里') +
  xlab(label = '船舶尺寸')#+ theme_bw()
# 船舶尺寸-航程
# 单位万海里
hours_size_bin = aggregate(sum_dt$gap_hours,by=list(sum_dt$size_bin,sum_dt$year),sum)%>%rename(.,'capacity'= 'Group.1','year'= 'Group.2','hours' = 'x')
hours_size_bin[,2] = as.character(distnm_size_bin$year)
hours_size_bin$hours  = hours_size_bin$hours/10000
hours_size_bin %>%
  ggplot(aes(fill=year,x=capacity, y=hours)) + 
  geom_bar(position="dodge", stat="identity",width=0.7,alpha=0.7) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_fill_manual(values = c("#3498db", "#1abc9c")) +
  ylab(label = '运营时间/万小时') +
  xlab(label = '船舶尺寸')#+ theme_bw()

# 用按日分类的文件
# 时间-航程
# 单位万海里

# yday2019=fread("~/论文数据/2019yday.csv",sep = ',')%>%na.omit(.)
yday2019=fread("/Volumes/Samsung\ SSD/汇总结果/fix/2019yday.csv",fill=TRUE)%>%na.omit(.)
yday2019 = as.data.table(aggregate(yday2019[,2:13],by=list(yday2019$yday),sum)%>%
                                                                        rename(.,'yday'= 'Group.1'))
yday2019 = yday2019[,year:=2019]

# fwrite(yday2019,paste('/Volumes/Samsung\ SSD/汇总结果/2019yday_sum.csv'),sep = ',',append = T)

# yday2020=fread("~/论文数据/2020yday.csv",sep = ',')%>%na.omit(.)
yday2020=fread("/Volumes/Samsung\ SSD/汇总结果/fix/2020yday.csv",fill=TRUE)%>%na.omit(.)
yday2020 = as.data.table(aggregate(yday2020[,2:13],by=list(yday2020$yday),sum)%>%
                           rename(.,'yday'= 'Group.1'))
yday2020 = yday2020[,year:=2020]
yday_dt =  rbind(yday2019,yday2020)
yday_dt$year = as.character(yday_dt$year)
yday_dt[,4:13]=yday_dt[,4:13]/1000000 # 单位吨


yday_dt %>%
  ggplot( aes(x=as.integer(x = yday),group = year, y = FC, color= year)) +
  geom_line() +
  ggtitle("") +
  theme_ipsum() +
  ylab("FC") +
  # geom_label( x=1990, y=55000, label="Amanda reached 3550\nbabies in 1970", size=4, color="#69b3a2") +
  theme( )

abnormal  = yday_dt[FC >= 130000 | FC <= 80000]

abnormal_AIS=fread("~/论文数据/abnormal.csv",sep = ',')

