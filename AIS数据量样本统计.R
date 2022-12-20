# AIS数据量样本统计
dir = "~/AIS/2019_year_container/" 
file_list <-  list.files(path = dir, recursive = T, include.dirs = TRUE, full.names = T)#%>%sub('\\.csv$', '',.)
file_num <- length(file_list)
nrow <- nrow(file_list