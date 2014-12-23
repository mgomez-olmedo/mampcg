library(bnlearn)

options(digits=2)

pathdb <- "/Users/mgomez/desarrollo/jmpenna/aprendizaje/ddbb/"
pathnet <- "/Users/mgomez/desarrollo/jmpenna/aprendizaje/redesFormatoNet/"
netName <- "asia"

sink()
sink()
sink("prueba-general")

# considera todos los posibles tamaños muestrales
samples <- c(500,1000,5000,10000,50000)

# se asigna valor para el numero de bbdd de cada tamaño muestral
repetitions <- 30

# se crea la matriz donde se almacenaran al final los resultados
resultsMean <- matrix(0,8,length(samples))
resultsDev <- matrix(0,8,length(samples))

cat("Comienzo de proceso de aprendizaje\n")

# se considera cada posible tamaño de muestra
for(i in 1:length(samples)){
  cat("Tratamiento de tamaño muestral: ",samples[i],"\n")
  pathdbsample <- ""
  # se compone el nombre del path donde buscar las bbdd
  pathdbsample <- paste(pathdb,netName,sep="")
  pathdbsample <- paste(pathdbsample,samples[i],sep="/")
  pathdbsample <- paste(pathdbsample,"/",sep="")
  cat("Path de busqueda de bases de datos: ",pathdbsample,"\n")
  
  # se crea matriz de resultados para cada tamaño muestral
  partialResults <- matrix(0,8,repetitions)
  
  # se considera cada posible variante
  for(j in 1:repetitions){
     model <- NULL
     modelPC <- NULL
     
     try(model <- learn(pathnet,pathdbsample,netName,j,samples[i],pc=FALSE))
     try(modelPC <- learn(pathnet,pathdbsample,netName,j,samples[i],pc=TRUE))
     cat("Fase de comparacion de estructuras.................\n")
     
     # ahora hay que comparar ambas estructuras
     if (!is.null(model)){
       vs <- model$compareStructure()
       cat("Aprendido modelo mampcg\n")
      
       # almacena los datos 
       partialResults[1,j] <- vs$recall
       if (!is.nan(vs$precision)){
         partialResults[2,j] <- vs$precision
       }
       partialResults[3,j] <- vs$recallVs
       if (!is.nan(vs$precisionVs)){
         partialResults[4,j] <- vs$precisionVs
       }
       
       storeNetResults(netName, "mampcg", samples[i], j, c(vs$recall,vs$precision,
                                                               vs$recallVs, vs$precisionVs))
     }
     
     if (!is.null(modelPC)){
       vsPC <- modelPC$compareStructure()
       cat("Aprendido modelo pc\n")
      
       # almacena los datos 
       partialResults[5,j] <- vsPC$recall
       if (!is.nan(vsPC$precision)){
         partialResults[6,j] <- vsPC$precision
       }
       partialResults[7,j] <- vsPC$recallVs
       if (!is.nan(vsPC$precisionVs)){
         partialResults[8,j] <- vsPC$precisionVs
       }
       
       storeNetResults(netName, "pc", samples[i], j, c(vsPC$recall,vsPC$precision,
                                                               vsPC$recallVs, vsPC$precisionVs))
     }
     
     cat("Finalizada ejecucion para muestra ",samples[i]," - ",j,"\n")
   }
  
   # calcula media y desviacion estandar de los resultados de cada
   # tamaño muestral
   cat("Valores parciales: \n")
   print(partialResults)
   resultsMean[,i] <- apply(partialResults,1,function(x){
     #xnc <- x[ x != 0]
     mean(x)
   })
  
   resultsDev[,i] <- apply(partialResults,1,function(x){
     #xnc <- x[x != 0]
     sd(x)
   })
  
   cat("Medias: \n")
   print(resultsMean)
   cat("Desviaciones: \n")
   print(resultsDev)
}

# procesamiento de resultados mediante metodo
resultsMampcg <- processResults(netName, "mampcg", samples, repetitions)
resultsPc <- processResults(netName, "pc", samples, repetitions)


sink()
sink()
options(digits=2)
traza <- paste(netName,"-results",sep="")
sink(traza)
options(digits=2)

# genera la tabla latex con los resultados
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\begin{tabular}{c|c|c|c|c|c|c|}")
cat("& &")
for(i in 1:length(samples)){
  cat(samples[i], " ")
  if (i != length(samples)){
    cat("&")
  }
}
cat("\\\\\\hline\n")

cat("\\multirow{4}{*}{mampcg}\n")

# recall data
cat("  & recall & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[1,i], " - ", resultsDev[1,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# precision data
cat("  & prec & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[2,i], " - ", resultsDev[2,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# recallVS data
cat("  & recallVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[3,i], " - ", resultsDev[3,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# prevVS data
cat("  & precVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[4,i], " - ", resultsDev[4,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{1-7}\n")

cat("\\multirow{4}{*}{pc}\n")

# recall data
cat("  & recall & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[5,i], " - ", resultsDev[5,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# precision data
cat("  & prec & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[6,i], " - ", resultsDev[6,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# recallVS data
cat("  & recallVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[7,i], " - ", resultsDev[7,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# prevVS data
cat("  & precVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMean[8,i], " - ", resultsDev[8,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{1-7}\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")
sink()
sink()
sink()

sink()
sink()
options(digits=2)
traza <- paste(netName,"-results2",sep="")
sink(traza)
options(digits=2)

# genera la tabla latex con los resultados
cat("\\begin{table}[h!]\n")
cat("\\centering\n")
cat("\\begin{tabular}{c|c|c|c|c|c|c|}")
cat("& &")
for(i in 1:length(samples)){
  cat(samples[i], " ")
  if (i != length(samples)){
    cat("&")
  }
}
cat("\\\\\\hline\n")

cat("\\multirow{4}{*}{mampcg}\n")

# recall data
cat("  & recall & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMampcg$mean[1,i], " - ", resultsPc$dev[1,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# precision data
cat("  & prec & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMampcg$mean[2,i], " - ", resultsMampcg$dev[2,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# recallVS data
cat("  & recallVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMampcg$mean[3,i], " - ", resultsMampcg$dev[3,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# prevVS data
cat("  & precVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsMampcg$mean[4,i], " - ", resultsMampcg$dev[4,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{1-7}\n")

cat("\\multirow{4}{*}{pc}\n")

# recall data
cat("  & recall & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsPc$mean[1,i], " - ", resultsPc$dev[1,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# precision data
cat("  & prec & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsPc$mean[2,i], " - ", resultsPc$dev[2,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# recallVS data
cat("  & recallVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsPc$mean[3,i], " - ", resultsPc$dev[3,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{3-7}\n")

# prevVS data
cat("  & precVS & ")

# salida de valores de recall para cada tamaño muestral
for(i in 1:length(samples)){
  cat(resultsPc$mean[4,i], " - ", resultsPc$dev[4,i])
  if (i != length(samples)){
    cat(" & ")
  }
}
cat("\\\\\\cline{1-7}\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")
sink()
sink()
sink()
