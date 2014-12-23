library(bnlearn)


learn <- function(pathnet, pathdb, netName, dbId, numberSamples, pc){
  # compose the name of the net
  netFileName <- paste(pathnet,netName,sep="")
  netFileName <- paste(netFileName,".net",sep="")
  
  # reads the net
  net <- read.net(netFileName,FALSE)
  
  # compose the name of the database
  dbName <- paste(netName,"-",sep="")
  dbName <- paste(dbName,numberSamples,sep="")
  dbName <- paste(dbName,"-",sep="")
  dbName <- paste(dbName,dbId,sep="")
  dbName <- paste(dbName,".db",sep="")
  dbCompleteName <- paste(pathdb,dbName,sep="")
  
  # reads the data base
  db <- readDataBase(dbCompleteName)
  
  # now learn the net for this db
  bInfo <- buildObject(net, db,pc=pc,debug=FALSE)
  
  # learn from this data
  bInfo$learn()
  
  # return bInfo
  return(bInfo)
}
