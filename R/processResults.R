
# process results
processResults <- function(netName, method, samples, repetitions){

  # se crea la matriz donde se almacenaran al final los resultados
  resultsMean <- matrix(0,4,length(samples))
  resultsDev <- matrix(0,4,length(samples))

  # considera cada posible tamaño de conjunto de datos
  for(i in 1:length(samples)){
    size <- samples[i]
  
    # se crea matriz de resultados para cada tamaño muestral
    partialResults <- matrix(0,4,repetitions)
  
    for(j in 1:repetitions){
      # se leen los datos de cada conjunto de datos
      result <- loadNetResults(netName, method, size, j)
    
      # se vuelcan sobre la matriz
      partialResults[,j] <- result[,1]
    }
  
    # now computes mean and deviation for this results
    for(j in 1:4){
      vals <- partialResults[j,]
    
      # counts all meaningful values (non cero, non NA)
      vals[is.na(vals)] <- 0
      
      # compute mean and deviation
      resultsMean[j,i] <- mean(vals)
      resultsDev[j,i] <- sd(vals)
    }
  }
 
  # return results
  return(list(mean=resultsMean,dev=resultsDev))
}