library('data.table')
library('dplyr') 
library('ggplot2')
library('hrbrthemes')
test=fread('/Users/mizexin/Desktop/ship_inventroy.csv')
# 排放量按种类加和
# colSums(is.na(test))

test=test[,total_CO2:=ME_CO2+AE_CO2+AB_CO2][,total_SOx:=ME_SOx+AE_SOx+AB_SOx][,total_BC:=ME_BC+AE_BC+AB_BC][,total_NOx:=ME_NOx+AE_NOx+AB_NOx][,total_CH4:=ME_CH4+AE_CH4+AB_CH4][,total_CO:=ME_CO+AE_CO+AB_CO][,total_N2O:=ME_N2O+AE_N2O+AB_N2O][,total_PM10:=ME_PM10+AE_PM10+AB_PM10][,total_PM2.5:=ME_PM2.5+AE_PM2.5+AB_PM2.5][,total_NMVOC:=ME_NMVOC+AE_NMVOC+AB_NMVOC];test

# 导出文档
# fwrite(test,'/Users/mizexin/Desktop/ship_inventroy_sum.csv')

排放量按列求总和
Emissions = c('CO2','SOx','BC','NOx','CH4','CO','N2O','PM10','PM2.5','NMVOC')
a = test %>%
  group_by(mode) %>%
  summarise(CO2=sum(total_CO2),
            SOx=sum(total_SOx),
            BC=sum(total_BC),
            NOx=sum(total_NOx),
            CH4=sum(total_CH4),
            CO=sum(total_CO),
            N2O=sum(total_N2O),
            PM10=sum(total_PM10),
            PM2.5=sum(total_PM2.5),
            NMVOC=sum(total_NMVOC))
b = test %>%
  summarise(CO2=sum(total_CO2),
            SOx=sum(total_SOx),
            BC=sum(total_BC),
            NOx=sum(total_NOx),
            CH4=sum(total_CH4),
            CO=sum(total_CO),
            N2O=sum(total_N2O),
            PM10=sum(total_PM10),
            PM2.5=sum(total_PM2.5),
            NMVOC=sum(total_NMVOC))
c = test %>%
  group_by(Ship_Type) %>%
  summarise(SOx=sum(total_SOx),
            BC=sum(total_BC),
            CH4=sum(total_CH4),
            CO2=sum(total_CO2),
            NOx=sum(total_NOx),
            CO=sum(total_CO),
            N2O=sum(total_N2O),
            PM10=sum(total_PM10),
            PM2.5=sum(total_PM2.5),
            NMVOC=sum(total_NMVOC))
# (CO2=sum(total_CO2),
# NOx=sum(total_NOx),
c <- as.matrix(c)
rownames(c) <- c[,1]
c <- as.matrix(c[,-1])
ze_barplot <- barplot(c,beside = T)
# 整理plot表格
sum_CO2 = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_CO2))
sum_CO2[,'EM_Type'] = 'CO2'

sum_SOx = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_SOx))
sum_SOx[,'EM_Type'] = 'SOx'

sum_BC = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_BC))
sum_BC[,'EM_Type'] = 'BC'

sum_NOx = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_NOx))
sum_NOx[,'EM_Type'] = 'NOx'

sum_CH4 = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_CH4))
sum_CH4[,'EM_Type'] = 'CH4'

sum_CO = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_CO))
sum_CO[,'EM_Type'] = 'CO'

sum_N2O = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_N2O))
sum_N2O[,'EM_Type'] = 'N2O'

sum_PM10 = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_PM10))
sum_PM10[,'EM_Type'] = 'PM10'

sum_PM2.5 = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_PM2.5))
sum_PM2.5[,'EM_Type'] = 'PM2.5'

sum_NMVOC = test %>%
  group_by(mode) %>%
  summarise(Value=sum(total_NMVOC))
sum_NMVOC[,'EM_Type'] = 'NMVOC'            
data = rbind(sum_CO2,sum_SOx,sum_BC,sum_NOx,sum_CH4,sum_CO,sum_N2O,sum_PM10,sum_PM2.5,sum_NMVOC)


# 出图
ggplot(data, aes(fill=mode, y=Value, x=EM_Type)) + 
  geom_bar(position="fill", stat="identity",alpha=0.7, width=0.6) +
  ylab('')+
  xlab('Emission type')+
  scale_fill_manual("MODE", values = c("#2ecc71", "#3498db","#34495e","#e67e22"))+
  ggtitle("Emisssion percentage") +
  scale_y_continuous(labels = scales::percent) + #纵坐标变为百分比
  theme_bw()+rremove('grid')+ # 标准主题模板
  xlab("")
########################################
mode_hours = test %>%
  group_by(mode) %>%
  summarise(Value=sum(hours))
shiptype_hours = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(hours))









## 速度分类求和
speed_CO2 = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_CO2))
speed_CO2[,'EM_Type'] = 'CO2'

speed_SOx = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_SOx))
speed_SOx[,'EM_Type'] = 'SOx'

speed_BC = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_BC))
speed_BC[,'EM_Type'] = 'BC'

speed_NOx = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_NOx))
speed_NOx[,'EM_Type'] = 'NOx'

speed_CH4 = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_CH4))
speed_CH4[,'EM_Type'] = 'CH4'

speed_CO = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_CO))
speed_CO[,'EM_Type'] = 'CO'

speed_N2O = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_N2O))
speed_N2O[,'EM_Type'] = 'N2O'

speed_PM10 = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_PM10))
speed_PM10[,'EM_Type'] = 'PM10'

speed_PM2.5 = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_PM2.5))
speed_PM2.5[,'EM_Type'] = 'PM2.5'

speed_NMVOC = test %>%
  group_by(speed) %>%
  summarise(Value=sum(total_NMVOC))
speed_NMVOC[,'EM_Type'] = 'NMVOC'            
data2 = rbind(speed_CO2,speed_SOx,speed_BC,speed_NOx,speed_CH4,speed_CO,speed_N2O,speed_PM10,speed_PM2.5,speed_NMVOC)

# library
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Plot
ggplot(data2, aes(x = speed, y = EM_Type, fill = Value)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
ggplot(data2, aes(x = speed, y = EM_Type, fill = Value)) +
  geom_density_ridges() +
  theme_ridges() 

## 不同船型分担率
Ship_Type_CO2 = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_CO2))
Ship_Type_CO2[,'EM_Type'] = 'CO2'

Ship_Type_SOx = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_SOx))
Ship_Type_SOx[,'EM_Type'] = 'SOx'

Ship_Type_BC = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_BC))
Ship_Type_BC[,'EM_Type'] = 'BC'

Ship_Type_NOx = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_NOx))
Ship_Type_NOx[,'EM_Type'] = 'NOx'

Ship_Type_CH4 = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_CH4))
Ship_Type_CH4[,'EM_Type'] = 'CH4'

Ship_Type_CO = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_CO))
Ship_Type_CO[,'EM_Type'] = 'CO'

Ship_Type_N2O = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_N2O))
Ship_Type_N2O[,'EM_Type'] = 'N2O'

Ship_Type_PM10 = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_PM10))
Ship_Type_PM10[,'EM_Type'] = 'PM10'

Ship_Type_PM2.5 = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_PM2.5))
Ship_Type_PM2.5[,'EM_Type'] = 'PM2.5'

Ship_Type_NMVOC = test %>%
  group_by(Ship_Type) %>%
  summarise(Value=sum(total_NMVOC))
Ship_Type_NMVOC[,'EM_Type'] = 'NMVOC'            
data = rbind(Ship_Type_SOx,Ship_Type_BC,Ship_Type_CH4,Ship_Type_CO,Ship_Type_N2O,Ship_Type_PM10,Ship_Type_PM2.5,Ship_Type_NMVOC)
data1 = rbind(Ship_Type_CO2,Ship_Type_NOx)
  # Ship_Type_CO2,Ship_Type_NOx
ggplot(data, aes(fill=EM_Type, y=Value, x=Ship_Type)) + 
  theme_bw()+rremove('grid')+
  geom_bar(position="dodge", stat="identity")
ggplot(data1, aes(fill=EM_Type, y=Value, x=Ship_Type)) + 
  theme_bw()+rremove('grid')+
  geom_bar(position="dodge", stat="identity")
