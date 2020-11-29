########################################################################
#' @author Sebastian Gaitan y Laura Herrera                            #
#' @description                                                        #
#' @param  SerieDeTiempo de un año a nivel nacional o departamental   #
#' @param  vec_eventos_lejanos eventos que queremos a analizar         #
#' @return                                                             #
########################################################################

###############
# LIBRERIAS:  #
###############
library(GGally); library(readxl); #library(ggplot2);
#library(dataset);library(tidyverse); 
rm(list=ls()); cat("\014"); 

##############
# FUNCIONES: #
##############
#Funcion 1
escribirSerieDeTiempo <- function(Clientes_venta, cliente){
  #Restringirnos solo a un cliente
  Datos <- subset(Clientes_venta, Clientes_venta$Cliente== cliente)
  
  #Modificar tabla para tener 24 meses
  n <- length(Datos[,1])
  for( i in c(1:n) ){
    if( (Datos[,1])[i] == 2020)
    {
      Datos[i,2] <- Datos[i,2] + 12
    }
  }

  Marca <- Datos$Marca2[!duplicated(Datos$Marca2)]
  
  #llenar vector de ceros
  vec_entradas_mat <- c()
  for( i in 1: (21* (length(Marca))) )
  {
    vec_entradas_mat <- c(vec_entradas_mat, 0)
  }
  
  #inicializar matriz
  Serie <- matrix(vec_entradas_mat, 21, length(Marca))
  colnames(Serie) <- Marca
  
  for (k in c(1:length(Marca)))
  {
    for (i in c(1:21) )
    {
      bool <- TRUE
      for(j in c(1:length(Datos$Mes)))
      {
        if( Datos$Marca2[j] == Marca[k])
        {
          if( Datos$Mes[j] == i)
          {
            Serie[i,k] <- Datos$Volumen[j]
            bool <- FALSE
          }
        }
      }
    }
  }
  
  return(Serie)
}
#Funcion 2
normalizar_columna <- function(Serie, i){
  M <- max(Serie[,i])
  if(M != 0)
  {
    for (j in c(1: length(Serie[,i])))
    {
      if (Serie[j,i]>0){
        Serie[j,i]<-1
      }
    }
  }
  
  return (Serie)
}
normalizar_serie <- function(Serie){
  for (i in c(1:ncol(Serie)))
  {
    Serie <- normalizar_columna(Serie, i)
  }
  return(Serie)
}
#Funcion 3
promedio <- function(Serie){
  vec_promedio <- c()
  for (i in c(1:ncol(Serie)))
  {
    aux <- 0
    for (j in c(1: nrow(Serie)))
    {
      aux <- aux + Serie[j,i]
    }
    aux <- aux/(nrow(Serie))
    
    vec_promedio <- c(vec_promedio, aux)
  }
  return(vec_promedio)
}
#Funcion 4 (calcula la regresion lineal)
reg_p <- function(Serie,k){
  vec_reg <- c()
  for (i in c(1:ncol(Serie)))
  {
    aux <- 0
    Vecn<- c(1:nrow(Serie))
    fit2 <- lm(Serie[,i]~poly(Vecn,k,raw=TRUE))
    aux<-predict(fit2, data.frame(Vecn=nrow(Serie)+1))
    if (aux>1) {
      aux<-1 
    }
    if (aux<0){
      aux<-0
    }
    vec_reg <- c(vec_reg, aux)
  }
  return(vec_reg)
}
#Funcion 5 (despues de resolver el modelo, escribe la probabilidad)
llenar_tabla_probabilidad_un_cliente <- function(Clientes_test, cliente, vec_promedio, vec_marcas,vec_nombres){
  j <- -1
  for(i in c(1:nrow(Clientes_test)))
  {
      if( Clientes_test$Cliente[i]== cliente )
      {
        j <- i
      }
  }
  if(j!=-1)
  {
    for (i in c(1:ncol(Clientes_test)))
    {
      for(k in c(1:length(vec_marcas)))
      {
        for(l in c(1:length(vec_nombres)))
        {
          if(vec_marcas[k] == vec_nombres[l])
          {
              Clientes_test[j,k+1] <- vec_promedio[l]
          }
        }
      }
    }
  }
  
  return(Clientes_test)
}

#Funcion Auxiliar
llenar_mat_ceros <- function(Mat){
  for(i in c(1:nrow(Mat))){
    #El siguiente for empieza en 2 para no anular el nombre de los clientes
    for(j in c(2:ncol(Mat))){
      Mat[i,j] <- 0
    }
  }
  return(Mat)
}


#########
# Main: #
#########

#Leer los datos
nombre_archivo <- paste("Clientes_venta_filtrados",".csv", sep="")
Clientes_venta <- read.csv(nombre_archivo, header=TRUE,sep=",")

nombre_archivo <- "Input3_clientes_test.csv"
Clientes_test <- read.csv(nombre_archivo, header=TRUE, sep=";")
Clientes_test <- llenar_mat_ceros(Clientes_test)

#Vector de marcas
vec_marcas <- c('Marca_9','Marca_16','Marca_20','Marca_38','Marca_39')

#Escoger tienda
vec_tiendas <- Clientes_venta$Cliente
vec_tiendas <- vec_tiendas [!duplicated(vec_tiendas)]
#View(vec_tiendas)
#for (l in c(1:20)){
l<-1
for( i in c(1:length(vec_tiendas))){
#for( i in c(1:20)){
  cliente <- vec_tiendas[i]
  #Escribir serie de tiempo
  SerieDeTiempo <- escribirSerieDeTiempo(Clientes_venta, cliente)
  SerieDeTiempoNormalizada <- normalizar_serie(SerieDeTiempo)
  
  #Hallar promedio de la tienda
  vec_promedio <- reg_p(SerieDeTiempoNormalizada , l)*(16/20) + promedio(SerieDeTiempoNormalizada)*(1-(16/20))

  #Vector de marcas qeu maneja la tienda  
  vec_tiendas_marcas <- colnames(SerieDeTiempoNormalizada)

  Clientes_test<-llenar_tabla_probabilidad_un_cliente(Clientes_test, cliente, vec_promedio, vec_marcas, vec_tiendas_marcas)
}
#Visualizar la tabla resultante
View(Clientes_test)
nombre_tabla <-paste("Tabla de probabilidad", ".csv", sep="")
#escribir el archivo
write.csv(Clientes_test,nombre_tabla, row.names = FALSE)