# 排放结果的目录
dir = "~/AIS/2019-stop-result" 
file_list = list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num <- length(file_list)
for (i in 1:file_num) {
  test=fread(file_list[i],sep = '')
  print(i)
}
test=fread('/Users/mizexin/AIS/2019-stop-result/477585400.csv' )
hist(test$ME_CO2 ,
     xlab = "ME_CO2 ",
     main = "",
     breaks = sqrt(nrow(test))) #无异常值
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
data = as.data.frame(rbind(cbind('ME_CO2EF',test$ME_CO2,test$yhour),cbind('ME_SO2EF',test$ME_SOx,test$yhour),cbind('ME_BC',test$ME_BC,test$yhour)))

# Plot
data %>%
  ggplot( aes(x=V1, y=V2, fill=V1)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.1) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")
data %>%
  ggplot( aes(x=V1, y=V2, fill=V1)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

ggplot(data=data, aes(x=V3,group=V1, fill=V2)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~V1) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )
