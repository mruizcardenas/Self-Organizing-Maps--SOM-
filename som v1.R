####Mapa autoorganizativo de Kohonen
####SOM
library(pheatmap)
heat<-function(matrix,name=" ",diag=TRUE){
  if(diag==FALSE){
    diag(matrix)<-0
  }
  pheatmap(matrix,color=colorRampPalette(c("white","black"))(1000),cluster_cols = FALSE, cluster_rows = FALSE,main =name)
  
}
inicializar<-function(n,m){
  ini<-matrix(0,1,n)
  for(i in 1:(m*m)){
    
    ini<-rbind(ini,c(rnorm(n)))
  }
  return(as.matrix(ini[-1,]))
}
numtocoords<-function(num,m){
  fila<-(num%/%m)+1
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
mod<-function(m,net,ele,modfactor=0.1){
  finalnet<-net
  r<-sum((net[1,]-ele)^2)
  min<-1
  for(j in 2:(m*m)){
    s<-sum((net[j,]-ele)^2)
    if(s<r){
      min<-j
      r<-s
    }
  }
  cmin<-numtocoords(min,m)
  modmatrix<-matrix(c(0,0,1,0,-1,0,0,1,0,-1,1,1,1,-1,-1,-1,-1,1),9,2,byrow = TRUE)
  for(i in 1:nrow(modmatrix)){
    coordstomod<-cmin+modmatrix[i,]
    if(sum((coordstomod<=m)*(coordstomod>=1))==2){
      numbertomod<-coordstonum(coordstomod[1],coordstomod[2],m)
      finalnet[numbertomod,]<-finalnet[numbertomod,]+modfactor*(ele-finalnet[numbertomod,])
    }
  }
  return(finalnet)
}
som_train<-function(traindata,m,epochs=1){
  n<-ncol(traindata)
  p<-nrow(traindata)
  #inicializar som
  net<-inicializar(n,m)
  net0<-net
  #entrenamiento
  for(i in 1:epochs){
    accv<-sample(p)
    for(j in accv){
      net<-mod(m,net,as.numeric(traindata[j,]))
    }
  }
  #salida
  returnlist<-list(net0,net)
  names(returnlist)<-c("ini","final")
  return(returnlist)
}
som_clustering<-function(testdata,trainednet,k){
  clustering_net<-as.numeric(kmeans(trainednet,centers=k)$cluster)
  clustering_final<-c()
  for(i in 1:nrow(testdata)){
    r<-sum((trainednet[1,]-testdata[i,])^2)
    min<-1
    for(j in 2:nrow(trainednet)){
      s<-sum((trainednet[j,]-testdata[i,])^2)
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
SOM<-som_train(datosprueba,m,epochs = 500)
S_0<-SOM$ini
S_1<-SOM$final
som_clustering(datosprueba,S_1,2)


setwd("C:/Users/Manuel Ruiz Cárdenas/Desktop/TFM/nuevos datos")
datosint<-read.csv("caso1_9.csv")
SOM<-som_train(datosint,6,epochs = 50)
S_0<-SOM$ini
S_1<-SOM$final
som_clustering(datosint,S_1,2)
heat(som_clustering(datosint,S_1,2))
heat(S_0)
heat(S_1)
