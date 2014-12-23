library(bnlearn)

# function for reading the data of the net from a file
readNetFile <- function(fileName,debug=FALSE){
  net <- read.net(fileName,debug)
}

# function for generating a random database from net
generateDataBases <- function(net, numberFiles, numberSamples, path, filename){
  # gets the file name without extension
  base.filename <- strsplit(filename, '[.]')[[1]][1]
  cat("Nombre base archivo: ",base.filename)
  
  # generates teh required number of databases
  for(i in 1:numberFiles){
    # generate the database
    db <- cpdist(net, nodes=nodes(net), evidence=TRUE, cluster = NULL, 
                 method = "ls", n=numberSamples, debug = FALSE)
    print(class(db))

    # stores in the corresponding file
    dbname <- paste(base.filename,numberSamples,sep="-")
    dbname <- paste(dbname,i,sep="-")
    dbname <- paste(dbname,".db",sep="")
    dbname <- paste(path,dbname,sep="")
    write.table(db,file=dbname,col.names=TRUE, row.names=FALSE,sep=",")
  }
}

# function for reading a database
readDataBase <- function(filedb){
  db <- read.csv(filedb,header=TRUE)
  colnames <- colnames(db)
  
  # converts everything into factor
  db[,colnames] <- lapply(db[,colnames] , factor)
  
  # return db
  return(db)
}



