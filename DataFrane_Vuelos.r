library("dplyr")
library("tidyverse")
require('geosphere')
library("tidyverse")

Calculo_Distancias <- function(Partida , Llegada ,Coordenadas ) {
  Aero_1 = which(Coordenadas$ana == Partida)
  Aero_2 = which(Coordenadas$ana == Llegada)
  distancia <- distHaversine(c(Coordenadas[Aero_1,2] , Coordenadas[Aero_1,3]) ,c(Coordenadas[Aero_2,2], Coordenadas[Aero_2,3])) / 1000# Para pasar a km  
  distancia <- as.numeric(round(distancia, 2))
  return(distancia)
}

aeropuertos_dataframe = read.csv2('sna_abril_2021.csv' , sep = ',' , encoding = "UTF-8")
vuelos_dataframe = read.csv2('202109-informe-ministerio.csv' , sep = ';' , stringsAsFactors = FALSE)

columnas_correcion <- c("cpr","fna", "nam", "nom_ciudad") 
aeropuertos_dataframe[columnas_correcion] <- lapply(aeropuertos_dataframe[columnas_correcion], gsub, pattern ="Ã­", replacement = "í")
aeropuertos_dataframe[columnas_correcion] <- lapply(aeropuertos_dataframe[columnas_correcion], gsub, pattern ="Ã©", replacement = "é")
aeropuertos_dataframe[columnas_correcion] <- lapply(aeropuertos_dataframe[columnas_correcion], gsub, pattern ="Ãġ", replacement = "ó")
aeropuertos_dataframe[columnas_correcion] <- lapply(aeropuertos_dataframe[columnas_correcion], gsub, pattern ="Ãẃ", replacement = "ú")
aeropuertos_dataframe[columnas_correcion] <- lapply(aeropuertos_dataframe[columnas_correcion], gsub, pattern ="ÃḂ", replacement = "á")

Coordenadas = aeropuertos_dataframe %>%
  select(ana, x, y)
Coordenadas$x <- as.numeric(as.character(Coordenadas$x))
Coordenadas$y <- as.numeric(as.character(Coordenadas$y))
summary(Coordenadas)
head(Coordenadas)

colnames(vuelos_dataframe) = c('Fecha' , 'Hora' , 'ClaseVuelo' , 'ClasificacionVuelo' , 'Tipomovimiento' , 
'Aeropuerto' , 'Origen_Destino' , 'Aerolinea' , 'Aeronave' , 'Pasajeros' , 'CalidadDato')

vuelos_dataframe = vuelos_dataframe[vuelos_dataframe$Aerolinea != 0,]
vuelos_dataframe = vuelos_dataframe[vuelos_dataframe$Aeronave != 0,]
vuelos_dataframe = vuelos_dataframe[vuelos_dataframe$Pasajeros != 0,]



cabotaje_aterrizaje <- subset(vuelos_dataframe , vuelos_dataframe$Tipomovimiento == 'Aterrizaje' & vuelos_dataframe$ClasificacionVuelo == 'Dom' & vuelos_dataframe$ClaseVuelo == 'Regular') 
cabotaje_despegue <- subset(vuelos_dataframe , vuelos_dataframe$Tipomovimiento == 'Despegue' & vuelos_dataframe$ClasificacionVuelo == 'Dom' & vuelos_dataframe$ClaseVuelo == 'Regular') 
cabotaje_aterrizaje = select(cabotaje_aterrizaje, -c('ClaseVuelo','ClasificacionVuelo','CalidadDato'))
cabotaje_despegue = select(cabotaje_despegue, -c('ClaseVuelo','ClasificacionVuelo','CalidadDato'))

head(cabotaje_despegue)

DF = inner_join(cabotaje_despegue,cabotaje_aterrizaje, by=c("Aeropuerto" = "Origen_Destino", "Origen_Destino" = "Aeropuerto" ,"Aerolinea" = "Aerolinea","Aeronave" = "Aeronave" , "Pasajeros" = "Pasajeros" ))

DF$Fecha_Hora_X = paste(as.Date(DF$Fecha.x, "%Y/%m/%d") ,DF$Hora.x , sep=" ")
DF$Fecha_Hora_Y = paste(as.Date(DF$Fecha.y, "%Y/%m/%d") ,DF$Hora.y , sep=" ")

DF$Tiempo_Vuelo = difftime(DF$Fecha_Hora_Y , DF$Fecha_Hora_X , unit = "hours")

DF_Final = filter(DF, Tiempo_Vuelo > 0.5 & Tiempo_Vuelo < 5)

head(DF_Final)

DF_Final = select(DF_Final, -c('Fecha.x','Hora.x','Tipomovimiento.x','Fecha.y','Hora.y','Tipomovimiento.y'))

head(DF_Final)

DF_Final$Distancia = 0

for (j in 1:nrow(DF_Final)){
    DF_Final[j,9] = Calculo_Distancias(DF_Final[j,1],DF_Final[j,2],Coordenadas)
    }


colnames(DF_Final) = c('Despegue' , 'Aterrizaje' , 'Aerolinea' , 'Aeronave' , 'Pasajeros' , 'Fecha Desp' ,
'Fecha Ater' , 'Tiempo Vuelo' , 'Distancia')


DF_Final

write.csv(DF_Final,'Vuelos.csv', row.names = FALSE)


