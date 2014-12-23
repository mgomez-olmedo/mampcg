library(bnlearn)

path <- "/Users/mgomez/desarrollo/jmpenna/aprendizaje/redesFormatoNet/"
pathddbb <- "/Users/mgomez/desarrollo/jmpenna/aprendizaje/ddbb/"

# method receving the name name of the net and generating
# the corresponding database
generateDataBasesForNet <- function(netFileName, numberFiles=1, numberSamples, 
                                    pathnet, pathdb){
  # composes the db name removing the extension ".net"
  # gets the file name without extension
  cat("Net: ",netFileName," samples: ",numberSamples," numberFiles: ", numberFiles, "\n")
  baseName <- strsplit(netFileName, '[.]')[[1]][1]
  dbFileName<- paste(baseName,".db",sep="")
  
  # composes path names for net and db
  netPathName <- paste(pathnet,netFileName,sep="")
  dbPathname <- paste(pathdb,baseName,sep="")
  dbPathname <- paste(dbPathname,"/",sep="")
  cat("Path de almacenamiento: ",dbPathname,"\n")
  if (!file.exists(dbPathname)){
    dir.create(dbPathname)
  }
  # into it creates a new subfolder for the number of samples
  dbPathname <- paste(dbPathname,numberSamples,sep="")
  dbPathname <- paste(dbPathname,"/",sep="")
  if (!file.exists(dbPathname)){
    dir.create(dbPathname)
  }
  cat("Path de almacenamiento con numero de muestras: ",dbPathname,"\n")
  
  # reads the net
  net <- readNetFile(netPathName,debug=FALSE)
  
  # generates the databases
  generateDataBases(net,numberFiles,numberSamples,dbPathname,dbFileName)
}

# generate data bases for all the nets
generateDataBasesForNets <- function(pathnet,pathdb, numberSamples, numberFiles){
  #nets <- list.files(pathnet,pattern="*.net",include.dirs=FALSE)
  nets <- c("asia.net")
  
  # generates data bases for every net
  sapply(nets,generateDataBasesForNet, numberFiles=numberFiles,
         numberSamples, pathnet=pathnet,pathdb=pathdb)
}

# generate data bases for all the nets
generateDataBasesForNetName <- function(pathnet,netname,pathdb,numberSamples){
  nets <- list.files(pathnet,pattern=netname,include.dirs=FALSE)
  print(class(nets))
  print(nets)
  
  # generates data bases for every net
  sapply(nets,generateDataBasesForNet, numberFiles=1,
         numberSamples=numberSamples, pathnet=pathnet,pathdb=pathdb)
}

# generateDataBasesForNetName(path,"mildew",pathddbb,1000)

# generates all the databases
ns <- c(500,1000,5000,10000,50000)
numberFiles <- 30
for(i in 1:length(ns)){
  nsamples <- ns[i]
  generateDataBasesForNets(path,pathddbb,nsamples,numberFiles)
}


