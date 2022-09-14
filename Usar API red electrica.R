# Bot de Telegram para la Extraccion diversos datos de la API de RED Eléctrica

remove(list=ls())

library(ggplot2)
library(telegram.bot)

# Initialize bot
bot <- Bot(token = bot_token("GuilleFBbot"))

updates <- bot$getUpdates()
# Get updates
while(length(updates)==0){
  updates <- bot$getUpdates()
}
  
#Sys.sleep(2)
# Retrieve your chat id
# Note: you should text the bot before calling `getUpdates`

# if (length(updates)==0){
#   chat_id=1499074368 # numero sin comillas
# } else {
#   chat_id = updates[[1L]]$from_chat_id()  
# }

chat_id = updates[[1L]]$from_chat_id() 
# Funciones del bot a preguntas del chat
mañana <- function(bot, update) {
  fecha=Sys.Date()+1
  temp=paste("https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real?start_date="
             ,fecha
             ,"T00:00&end_date="
             ,fecha
             ,"T24:00&time_trunc=hour"
             #&geo_ids=8743" #geo_ids de baleares
             ,sep="")
  
  json_file=jsonlite::fromJSON(temp)
  
  data=as.data.frame(json_file$included$attributes$values[[1]])
  data$value=data$value/1000
  
  data$datetime=as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  # 
  # ggplot(data = data, aes(x = datetime, y = value))+
  #   ylim(c(0,500))+
  #   geom_point(color = "#00AFBB", size = 2)
  # 
  # plot(data$datetime,data$value,ylim=c(0,500))
  
  # data$datetime[which(data$value==min(data$value))]
  # data$datetime[which(data$value==max(data$value))]
  
  # Las horas mas baratas
  Baratas=data[order(data$value)[1:3],c(1,3)]
  a=as.POSIXlt(Baratas$datetime)
  
  # Las horas mas caras
  Caras=data[order(data$value, decreasing = T)[1:3],c(1,3)]
  b=as.POSIXlt(Caras$datetime)
  
  texto=paste("Mañana ", "*",as.character(fecha, format="%A, %d de %B de %Y"), "*",
              ", las horas mas baratas son: ", "*", paste(a$hour, collapse = ", "),"*",
              " y las horas mas caras son: ", "*",paste(b$hour, collapse = ", "), "*",
              sep = "")
  
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = texto, parse_mode = "Markdown"
  )
}

###########################
hoy <- function(bot, update) {
  fecha=Sys.Date()
  temp=paste("https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real?start_date="
             ,fecha
             ,"T00:00&end_date="
             ,fecha
             ,"T24:00&time_trunc=hour"
             #&geo_ids=8743" #geo_ids de baleares
             ,sep="")
  
  json_file=jsonlite::fromJSON(temp)
  
  data=as.data.frame(json_file$included$attributes$values[[1]])
  data$value=data$value/1000
  
  data$datetime=as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  # 
  # ggplot(data = data, aes(x = datetime, y = value))+
  #   ylim(c(0,500))+
  #   geom_point(color = "#00AFBB", size = 2)
  # 
  # plot(data$datetime,data$value,ylim=c(0,500))
  
  # data$datetime[which(data$value==min(data$value))]
  # data$datetime[which(data$value==max(data$value))]
  
  # Las horas mas baratas
  Baratas=data[order(data$value)[1:3],c(1,3)]
  a=as.POSIXlt(Baratas$datetime)
  
  # Las horas mas caras
  Caras=data[order(data$value, decreasing = T)[1:3],c(1,3)]
  b=as.POSIXlt(Caras$datetime)
  
  texto=paste("Hoy ", "*",as.character(fecha, format="%A, %d de %B de %Y"), "*",
              ", las horas mas baratas son: ", "*", paste(a$hour, collapse = ", "),"*",
              " y las horas mas caras son: ", "*",paste(b$hour, collapse = ", "), "*",
              sep = "")
  
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = texto, parse_mode = "Markdown"
  )
}

###########################
start <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = sprintf("Hello %s!", update$message$from$first_name)
  )
}

#######################
preciosmañana <- function(bot, update) {
  fecha=Sys.Date()+1
  temp=paste("https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real?start_date="
             ,fecha
             ,"T00:00&end_date="
             ,fecha
             ,"T24:00&time_trunc=hour"
             #&geo_ids=8743" #geo_ids de baleares
             ,sep="")
  
  json_file=jsonlite::fromJSON(temp)
  
  data=as.data.frame(json_file$included$attributes$values[[1]])
  data$value=data$value/1000
  
  data$datetime=as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  #
  # ggplot(data = data, aes(x = datetime, y = value))+
  #   ylim(c(0,500))+
  #   geom_point(color = "#00AFBB", size = 2)
  #
  # plot(data$datetime,data$value,ylim=c(0,500))
  
  # data$datetime[which(data$value==min(data$value))]
  # data$datetime[which(data$value==max(data$value))]
  
  # Las horas mas baratas
  Baratas=data[order(data$value)[1:3],c(1,3)]
  a=as.POSIXlt(Baratas$datetime)
  
  # Las horas mas caras
  Caras=data[order(data$value, decreasing = T)[1:3],c(1,3)]
  b=as.POSIXlt(Caras$datetime)
  
  # texto=paste("Mañana ", "*",as.character(fecha, format="%A, %d de %B de %Y"), "*",
  #             ", las horas mas baratas son: ", "*", paste(a$hour, collapse = ", "),"*",
  #             " y las horas mas caras son: ", "*",paste(b$hour, collapse = ", "), "*",
  #             sep = "")
  
  texto=paste("*Precios por hora para el:*",as.character(fecha, format="%A, %d de %B de %Y"),
              paste(paste(sprintf("%.3f",round(data[,1],4)),as.data.frame(strsplit(as.character(data[,3]), " "))[2,],sep="  "),collapse = "\n"), sep="\n")
  
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = texto, parse_mode = "Markdown"
  )
  
}

####################

precioshoy <- function(bot, update) {
  fecha=Sys.Date()
  temp=paste("https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real?start_date="
             ,fecha
             ,"T00:00&end_date="
             ,fecha
             ,"T24:00&time_trunc=hour"
             #&geo_ids=8743" #geo_ids de baleares
             ,sep="")
  
  json_file=jsonlite::fromJSON(temp)
  
  data=as.data.frame(json_file$included$attributes$values[[1]])
  data$value=data$value/1000
  
  data$datetime=as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  #
  # ggplot(data = data, aes(x = datetime, y = value))+
  #   ylim(c(0,500))+
  #   geom_point(color = "#00AFBB", size = 2)
  #
  # plot(data$datetime,data$value,ylim=c(0,500))
  
  # data$datetime[which(data$value==min(data$value))]
  # data$datetime[which(data$value==max(data$value))]
  
  # Las horas mas baratas
  Baratas=data[order(data$value)[1:3],c(1,3)]
  a=as.POSIXlt(Baratas$datetime)
  
  # Las horas mas caras
  Caras=data[order(data$value, decreasing = T)[1:3],c(1,3)]
  b=as.POSIXlt(Caras$datetime)
  
  # texto=paste("Mañana ", "*",as.character(fecha, format="%A, %d de %B de %Y"), "*",
  #             ", las horas mas baratas son: ", "*", paste(a$hour, collapse = ", "),"*",
  #             " y las horas mas caras son: ", "*",paste(b$hour, collapse = ", "), "*",
  #             sep = "")
  
  texto=paste("*Precios por hora para el:*",as.character(fecha, format="%A, %d de %B de %Y"),
              paste(paste(sprintf("%.3f",round(data[,1],4)),as.data.frame(strsplit(as.character(data[,3]), " "))[2,],sep="  "),collapse = "\n"), sep="\n")
  
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = texto, parse_mode = "Markdown"
  )
  
}

####################

# Send photo
fotocasa <- function(bot, update) {
  url.foto = paste("http://admin:admin@192.168.1.31:88/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2&usr=",
                   unlist(strsplit(Sys.getenv("UserPassCamCasa"),":"))[1],
                   "&pwd=",
                   unlist(strsplit(Sys.getenv("UserPassCamCasa"),":"))[2],
                   sep="")
  Sys.sleep(2)
  foto.name=paste(gsub(x = as.character(Sys.time()), pattern = ":", replacement = "_"), '.jpg', sep="")
  invisible()
  download.file(url.foto, foto.name, mode = 'wb', quiet = TRUE)
  Sys.sleep(2)
  bot$sendDocument(chat_id,
                   document = paste(getwd(),"/",foto.name,sep="")
  )
}

####################

updater <- Updater(bot_token("GuilleFBbot")) +
  CommandHandler("mañana", mañana) +
  CommandHandler("hoy", hoy) +
  CommandHandler("preciosmañana", preciosmañana) +
  CommandHandler("precioshoy", precioshoy) +
  CommandHandler("fotocasa", fotocasa) +
  CommandHandler("start", start)

updater$start_polling()
