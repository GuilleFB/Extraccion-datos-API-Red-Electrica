# Extraccion diversos datos de la API de RED Eléctrica

remove(list=ls())

library(ggplot2)

temp=paste("https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real?start_date="
           ,Sys.Date()+1
           ,"T00:00&end_date="
           ,Sys.Date()+1
           ,"T24:00&time_trunc=hour"
           #&geo_ids=8743" #geo_ids de baleares
           ,sep="")

json_file=jsonlite::fromJSON(temp)

data=as.data.frame(json_file$included$attributes$values)

data$datetime=as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")

ggplot(data = data, aes(x = datetime, y = value))+
  ylim(c(0,450))+
  geom_point(color = "#00AFBB", size = 2)

plot(data$datetime,data$value,ylim=c(0,500))

# data$datetime[which(data$value==min(data$value))]
# data$datetime[which(data$value==max(data$value))]

# Las dos horas mas baratas
data$datetime[sort(order(data$value)[1:3])]

# Las dos horas mas caras
data$datetime[sort(order(data$value,decreasing = T)[1:2])]
