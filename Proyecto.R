library(readxl)
library(lubridate)
library(ChainLadder)
library(dplyr)

#------------- Lectura de la base de datos-------------------------------------

primas <- read_excel("Datos Proyecto.xlsx",
                        sheet = "Primas emitidas")
datos <-  read_excel("Datos Proyecto.xlsx",
                     sheet = "Siniestros pagados")

##--- Primas cobradas-------------------

# Convertir las fechas a formato Date

primas$MES_EMISION <- as.Date(primas$MES_EMISION)
primas$MES_EMISION <-  format(primas$MES_EMISION, "%Y-%m")

# Agrupar por Mes y Año y sumar el monto de las primas
primas_por_mes <- primas %>%
  group_by(MES_EMISION) %>%
  summarise(PRIMA = sum(PRI_LOCAL, na.rm = TRUE))
primas_por_mes <- primas_por_mes[-nrow(primas_por_mes),]


##---Siniestros pagados-----------------

# Convertir las fechas a formato Date
datos$FECOCURR <- as.Date(datos$FECOCURR)
datos$FECPAGO <- as.Date(datos$FECPAGO)

#Filtrar la base de datos
datos_filtrado <- datos[
  year(datos$FECOCURR) >= 2022 & year(datos$FECPAGO) >= 2022 & 
    !(year(datos$FECOCURR) == 2024 & month(datos$FECOCURR) == 8) & 
    !(year(datos$FECPAGO) == 2024 & month(datos$FECPAGO) == 8), 
]

# Extraer año y mes de FECOCURR y FECPAGO
año_ocurrencia <-as.numeric(format(datos_filtrado$FECOCURR,"%Y"))
mes_ocurrencia <- as.numeric(format(datos_filtrado$FECOCURR, "%m"))
año_pago <- as.numeric(format(datos_filtrado$FECPAGO, "%Y"))
mes_pago <- as.numeric(format(datos_filtrado$FECPAGO, "%m"))

# Calcular la diferencia en meses 
datos_filtrado$`Periodo Desarrollo Meses` <- (año_pago - año_ocurrencia) * 12 + 
  (mes_pago - mes_ocurrencia)

datos_filtrado$FECOCURR<- format(datos_filtrado$FECOCURR, "%Y-%m")
datos_filtrado$FECPAGO<- format(datos_filtrado$FECPAGO, "%Y-%m")

#Se crea el triángulo Run Off. 
triangulo <- as.triangle(
  datos_filtrado, 
  origin = "FECOCURR",
  dev = "Periodo Desarrollo Meses",
  value = "MTOPAG_LOCAL"
)