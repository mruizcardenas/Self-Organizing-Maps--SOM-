####Mapa autoorganizativo de Kohonen
####SOM v2.1

#arquitectura de las neuronas bidimensional rectangular, hexagonal
#o pseudoaleatoria de número y organización variables

#inicialización de las neuronas de forma aleatoria

#mejora de las neuronas fija teniendo en cuenta radios de acción fijos

#clustering final obtenido aplicando k-means a los valores de la red 
#entrenados

#La posición virtual de las neuronas es fija y forma parte de ellas

library(pheatmap)
heat<-function(matrix,name=" ",diag=TRUE){
  if(diag==FALSE){
    diag(matrix)<-0
  }
  pheatmap(matrix,color=colorRampPalette(c("white","black"))(1000),cluster_cols = FALSE, cluster_rows = FALSE,main =name)
  
}
mesh<-function(l,a,type="grid"){
  objreturn<-"errortype"
  if(type=="grid"){
    if(l%%a==0){
      fila<-l%/%a
      columna<-a
    }else{
      fila<-(l%/%a)+1
      columna<-l%%a
    }
    objreturn<-c(columna,fila)
  }
  
  if(type=="hexa"){
    if(l%%a==0){
      fila<-l%/%a
      columna<-a
    }else{
      fila<-(l%/%a)+1
      columna<-l%%a
    }
    if(fila%%2==1){
      despx<-0
    }else{
      despx<-0.5
    }
    
    objreturn<-c(columna+despx,1+(fila-1)*(sqrt(3)/2))
  }
  
  if(type=="randgrid"){
    if(l%%a==0){
      fila<-l%/%a
      columna<-a
    }else{
      fila<-(l%/%a)+1
      columna<-l%%a
    }
    objreturn<-c(columna+runif(1,min=-0.3,max=0.3),fila+runif(1,min=-0.3,max=0.3))
  }
    
  return(objreturn)
}

inicializar<-function(n,p,a,meshtype="grid"){
  ininode<-matrix(0,1,n)
  inicoord<-matrix(0,1,2)
  for(i in 1:(p)){
    ininode<-rbind(ininode,c(rnorm(n)))
    inicoord<-rbind(inicoord,mesh(i,a,meshtype))
  }
  ininode<-as.matrix(ininode[-1,])
  inicoord<-as.matrix(inicoord[-1,])
  listreturn<-list(ininode,inicoord)
  names(listreturn)<-c("nodo","coord")
  return(listreturn)
}
numtocoords<-function(num,m){
  if(num%%m==0){
    fila<-num%/%m
    columna<-m
  }else{
    fila<-(num%/%m)+1
    columna<-num%%m
  }
  return(c(fila,columna))
}
coordstonum<-function(x,y,m){
  return(((x-1)*m)+y)
}
mod<-function(net,ele,modfactor=0.1){
  m<-nrow(net$nodo)
  finalnet<-net
  r<-sum((net$nodo[1,]-ele)^2)
  min<-1
  for(j in 2:(m)){
    s<-sum((net$nodo[j,]-ele)^2)
    if(s<r){
      min<-j
      r<-s
    }
  }
  cmin<-net$coord[min,]
  for(i in 1:m){
    if(sum((cmin-finalnet$coord[i,])^2)<=1){
      finalnet$nodo[i,]<-finalnet$nodo[i,]+modfactor*(ele-finalnet$nodo[i,])
    }
  }
  return(finalnet)

}

som_train<-function(traindata,p,a,epochs=1,meshtype="grid"){
  n<-ncol(traindata)
  q<-nrow(traindata)
  #inicializar som
  net<-inicializar(n,p,a,meshtype)
  net0<-net
  #entrenamiento
  for(i in 1:epochs){
    accv<-sample(q)
    for(j in accv){
      net<-mod(net,as.numeric(traindata[j,]))
    }
  }
  #salida
  returnlist<-list(net0,net)
  names(returnlist)<-c("ini","final")
  return(returnlist)
}
som_clustering<-function(testdata,trainednet,k){
  clustering_net<-as.numeric(kmeans(trainednet$nodo,centers=k)$cluster)
  clustering_final<-c()
  for(i in 1:nrow(testdata)){
    r<-sum((trainednet$nodo[1,]-testdata[i,])^2)
    min<-1
    for(j in 2:nrow(trainednet$nodo)){
      s<-sum((trainednet$nodo[j,]-testdata[i,])^2)
      if(s<r){
        min<-j
        r<-s
      }

    }
    clustering_final<-c(clustering_final,clustering_net[min])
  }
  return(clustering_final)
}
v<-rep(1,10)
w<-rep(-1,10)
datosprueba<-as.matrix(rbind(v,v,w,w))
m<-4
SOM<-som_train(datosprueba,p=4,a=2,epochs = 500,meshtype = "hexa")
S_0<-SOM$ini
S_1<-SOM$final
som_clustering(datosprueba,S_1,2)


setwd("C:/Users/Manuel Ruiz Cárdenas/Desktop/TFM/nuevos datos")
datosint<-read.csv("caso1_2.csv")
SOM<-som_train(datosint,p=16,a=1,epochs = 50, meshtype = "hexa")
S_0<-SOM$ini
S_1<-SOM$final
som_clustering(datosint,S_1,2)



