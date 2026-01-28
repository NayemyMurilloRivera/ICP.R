IC_media_ <- function() {
  desviacion <- menu(
    c("Desviación poblacional conocida",
      "Desviación poblacional desconocida"),
    title = "Tipo de desviación"
  )
  
  nivel_conf <- as.numeric(readline("Ingrese el nivel de confianza en decinales: "))
  a <- 1 - nivel_conf
  
  opcion <- menu(
    c("Ingresar parámetros y/o estadísticos", "Ingresar datos muestrales"
    ),
    title = "Seleccione ")
  
  if (opcion == 1) {
    media <- as.numeric(readline("Ingrese la media muestral: "))
    n <- as.numeric(readline("Ingrese el tamaño de muestra n: "))
    if(desviacion==1){s <- as.numeric(readline("Ingrese la desviación poblacional: "))}
    else {s <- as.numeric(readline("Ingrese la desviación muestral: "))}
    if(n>=30){error<-qnorm(1-a/2)*s/sqrt(n)
    distribucion <- "T de Student"}
    else{error<-qnorm(1-a/2)*s/sqrt(n)
    distribucion <- "Normal"}
  }
  
  if (opcion == 2) {
    datos <- scan(what = numeric(), quiet = TRUE)
    n <- length(datos)
    media <- mean(datos)
    if(desviacion==1){s <- as.numeric(readline("Ingrese la desviación poblacional: "))}
    else {  s <- sd(datos)}
    if(n>=30){error<-qnorm(1-a/2)*s/sqrt(n)
    distribucion <- "T de Student"}
    else{error<-qnorm(1-a/2)*s/sqrt(n)
    distribucion <- "Normal"}
  }
  poblacion <- menu(
    c("Si", "No"),
    title = "¿La población es finita?"
  )
  
  if(poblacion==1){
    N <- as.numeric(readline("Ingrese el tamaño de la población: "))
    error<-error*sqrt((N-n)/(N-1))}
  
  LI<-round(media-error,5)
  LS<- round(media+error,5)
  cat("Media :", round(media, 4), "\n")
  cat("Distribución usada:", distribucion, "\n")
  cat(paste0("Con un nivel de confianza de ", nivel_conf*100, "%", 
             " la media poblacional \n se encuentra en el intervalo [",LI,";", LS,"]" )
  )
  
}
