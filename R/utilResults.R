
# stores the data of a learning process on netName with
# a certain method, number of samples, id of data base file
storeNetResults <- function(netName, method, samples, id, results){
  # se comprueba si existe el directorio de la red en la carpeta de
  # resultados
  basicPath <- "./results/"
  path <- paste(basicPath,netName,sep="")
  cat("basicPath paso 1: ",basicPath,"/")
  cat("path paso 1: ",path,"\n")
  if (!file.exists(path)){
    dir.create(file.path(basicPath, netName))    
  }
  basicPath <- path
  
  # se comprueba si existe el directorio para el metodo
  path <- paste(basicPath,method,sep="/")
  cat("basicPath paso 2: ",basicPath,"/")
  cat("path paso 2: ",path,"\n")  
  if (!file.exists(path)){
    dir.create(file.path(basicPath, method))    
  }
  basicPath <- path
  
  # se comprueba si existe el directorio para el numero de muestras
  # de interes
  path <- paste(basicPath,samples,sep="/")
  path <- paste(path,"/",sep="")
  cat("basicPath paso 3: ",basicPath,"/")
  cat("path paso 3: ",path,"\n")  
  if (!file.exists(path)){
    dir.create(file.path(basicPath, samples))    
  }
  
  # ahora se escribe el archivo en disco
  fileName <- paste(netName,"-",sep="")
  fileName <- paste(fileName,samples,sep="")
  fileName <- paste(fileName,"-",sep="")
  fileName <- paste(fileName,id,sep="")
  fileName <- paste(fileName,".dat",sep="")
  finalPath <- paste(path,fileName,sep="")
  cat("finalPath: ",finalPath,"\n")
  write.table(results, file = finalPath, row.names=FALSE, col.names=FALSE, sep=",")
}

loadNetResults <- function(netName, method, samples, id){
  # compone el nombre del path
  basicPath <- "./results"
  path <- paste(basicPath,netName,sep="/")
  path <- paste(path,method,sep="/")
  path <- paste(path,samples,sep="/")
  path <- paste(path,netName,sep="/")
  path <- paste(path,samples,sep="-")
  path <- paste(path,id,sep="-")
  path <- paste(path,".dat",sep="")
  cat("Archivo de resultados: ",path,"\n")
  
  # se lee el archivo
  data <- as.data.frame(c(0,0,0,0))
  if (file.exists(path)){
     data <- read.csv(path,header=FALSE)
  }
  return(data)
}

storeNetResults("alarm","pc",100,1,c(1,2,3,4))
data <- loadNetResults("alarm","pc",100,1)

