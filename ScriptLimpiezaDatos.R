########################################################################
#' @author Sebastian Gaitan y Laura Herrera                            #
#' @description                                                        #
#' @param  SerieDeTiempo de un aÃ±o a nivel nacional o departamental   #
#' @param  vec_eventos_lejanos eventos que queremos a analizar         #
#' @return                                                             #
########################################################################

###############
# LIBRERIAS:  #
###############
library(GGally); 
library(readxl); 
library(tidyverse);
library(dplyr);
library(dataset);
rm(list=ls()); cat("\014"); 

##################
# LIMPIAR DATOS: #
##################

#PRIMER INPUT
  #Leer los datos
  nombre_archivo <- "Input1_clientes_estructura.csv"
  Clientes_estructura <- read.csv(nombre_archivo, header=TRUE, sep=";")
  View(Clientes_estructura)

#SEGUNDO INPUT
  #Leer los datos
  nombre_archivo <- "Input2_clientes_venta.csv"
  Clientes_venta <- read.csv(nombre_archivo, header=TRUE, sep=";")
  View(Clientes_venta)
  
  Clientes_venta <- as.data.frame(Clientes_venta)
  
  Clientes_venta <- Clientes_venta %>%
    select(-SegmentoPrecio2, -disc, -nr)
  
  #Restringirnos solo a los productos de interés
  #Datos <-subset(Mis_datos, Mis_datos$COD_DPTO_O== dpto)
  vec_marcas <- c("Marca_9","Marca_16","Marca_20","Marca_38","Marca_39")
  Clientes_venta_filtrados <- Clientes_venta %>% 
                                filter (Marca2 %in% c("Marca_9","Marca_16","Marca_20","Marca_38","Marca_39"))
  
  Clientes_venta_filtrados <- Clientes_venta_filtrados %>% 
    filter ( (Marca2 == "Marca_9" & Cupo2 == "Cupo_3" & CapacidadEnvase2 == "CapacidadEnvase_12")
             | (Marca2 == "Marca_16" & Cupo2 == "Cupo_2" & CapacidadEnvase2 == "CapacidadEnvase_10")
             | (Marca2 == "Marca_20" & Cupo2 == "Cupo_3" & CapacidadEnvase2 == "CapacidadEnvase_9")
             | (Marca2 == "Marca_38" & Cupo2 == "Cupo_2" & CapacidadEnvase2 == "CapacidadEnvase_10")
             | (Marca2 == "Marca_39" & Cupo2 == "Cupo_2" & CapacidadEnvase2 == "CapacidadEnvase_10"))
  View(Clientes_venta_filtrados)
  
  nombre_tabla <- paste("Clientes_venta_filtrados",".csv", sep="")
  write.csv(Clientes_venta_filtrados,nombre_tabla, row.names = FALSE)
  
  
#TERCER INPUT
  #Leer los datos
  nombre_archivo <- "Input3_clientes_test.csv"
  Clientes_test <- read.csv(nombre_archivo, header=TRUE, sep=";")
  View(Clientes_test)
  