dt0=fread("/Volumes/Samsung\ SSD/single.csv",sep = ',',select = c('yday','ME_FC','AE_FC'))

dt1=fread("/Volumes/Samsung\ SSD/single_fix.csv",sep = ',',select = c('yday','ME_FC','AE_FC'))



dt0 = as.data.table(aggregate(dt0[,2:3],by=list(dt0$yday),sum)%>%
                     rename(.,'yday'= 'Group.1'))
dt0$fix='before'
dt1 = as.data.table(aggregate(dt1[,2:3],by=list(dt1$yday),sum)%>%
                      rename(.,'yday'= 'Group.1'))
dt1$fix='after'
a=rbind(dt0,dt1)
a |> 
  group_by(fix) |>
  e_charts(x = yday) |> # initialise and set x
  e_area(ME_FC)  |># add a line
  e_tooltip(trigger = "axis") # tooltip
