#Declaracion de librerias
library('fractal')
library('fArma')
library('ggplot2')
library('gridExtra')

set.seed(123)


#################################
# Funciones                     #
#################################

#---Retorna un vector con n modelos con el coeficiente HG
generateSignals<-function(HGvalue,items,modelSignal){
 
  re=c()
  r2<-list()
  model<-lmModel(modelSignal,HG=HGvalue)
  for(n in 1:items)
  {
    #r$size<-lmSimulate(model)
    r2[[n]] <-as.vector(lmSimulate(model))
  }
  r2     
}
#--------------------------------------

#--Retorna la estimación el el dominio del tiempo
getTimeEstimation<-function(x){
    r<-hurstBlock(x, method="aggAbs", scale.min=8, scale.max=NULL,
               scale.ratio=2, weight=function(x) rep(1,length(x)), fit=lm)
    unname(r)
  }
#----------------------------------------------
#--Retorna la estimación en el dominio de la frecuencia--#
getFrequencyEstimation<-function(x){
  r<-hurstSpec(x, method="standard", freq.max=0.25, dc=FALSE, n.block=NULL,
               weight=function(x) rep(1,length(x)), fit=lm, sdf.method="direct")
  unname(r)
}

#--------------------------------------------------------#
#--Retorna la estimación en el dominio de tiempo-escala--#
getTimeScaleEstimation<-function(x){
  r<-waveletFit(x, length = NULL, order = 2, octave = c(2, 8),doplot = FALSE, title = NULL, description = NULL)#FDWhittle(x, method="continuous", dc=FALSE, freq.max=0.5,delta.min=-1,delta.max=2.5, sdf.method="direct")
  foo<-unname(r@hurst[[1]])
  #print(foo)
  unname(r@hurst[[1]])
}
#--------------------------------------------------------#

#--Retorna el valor MSE

mse<-function(x){
  r<-var(x)/length(x)
  return(r)
}


#####
fractalMain<-function(n)
{
  hgValues<-seq(0.1,0.9,by=0.1)
  timeEstimation=c()
  frequencyEstimation=c()
  weibletsEstimation=c()
  matriz <-matrix(0,9,9)
  mBoxPlot<-matrix(,nrow = n, ncol = 9)#matrix(ncol=27,nrow=9)
  mBoxPlot2<-matrix(,nrow = n, ncol = 9)
  mBoxPlot3<-matrix(,nrow = n, ncol = 9)
  estimationsList<-list()
  df <- data.frame(Foo=NULL)
  #pdf('final2.pdf')
  index=1
  index2=1
  index3=1
  for(i in hgValues){
    tmp<- generateSignals(i,n,"fgn")
    for(j in 1:n)
    {
      timeEstimation[j]<-getTimeEstimation(tmp[[j]])[1]
      #print(getTimeEstimation(tmp[[j]])[1])
      frequencyEstimation[j]<-getFrequencyEstimation(tmp[[j]])[1]
      weibletsEstimation[j]<-getTimeScaleEstimation(tmp[[j]])
    }
    fr<-paste("Name", i, sep="")
    #print(fr)
    #df[fr]<- NA
    #df$Foo<-timeEstimation
    matriz[index,1] = format(round(mean(timeEstimation), 3), nsmall = 3)#
    matriz[index,2] = format(round(var(timeEstimation), 3), nsmall = 3)#floor(var(timeEstimation))
    matriz[index,3] = format(round(mse(timeEstimation), 3), nsmall = 3)#floor(mse(timeEstimation))
    
    matriz[index,4] = format(round(mean(frequencyEstimation), 3), nsmall = 3)#floor(mean(frequencyEstimation))
    matriz[index,5] = format(round(var(frequencyEstimation), 3), nsmall = 3)#floor(var(frequencyEstimation))
    matriz[index,6] = format(round(mse(frequencyEstimation), 3), nsmall = 3)#floor(mse(frequencyEstimation))
    
    #print(weibletsEstimation)
    matriz[index,7] =format(round(mean(weibletsEstimation), 3), nsmall = 3)#floor(mean(frequencyEstimation))
    matriz[index,8] = format(round(var(weibletsEstimation), 3), nsmall = 3)#floor(var(weibletsEstimation))
    matriz[index,9] = format(round(mse(weibletsEstimation), 3), nsmall = 3)#floor(mse(weibletsEstimation))
    
    
    #estimationsList[index]<-timeEstimation
    #estimationsList[index+1]<-frequencyEstimation
    #estimationsList[index+2]<-weibletsEstimation
    if(index <= 3){
      indexRel<-index*3
      print(cat("SE extiende a: ",indexRel))
    
      mBoxPlot[,indexRel]<-timeEstimation
      mBoxPlot[,indexRel-1]<-frequencyEstimation
      mBoxPlot[,indexRel-2]<-weibletsEstimation
      print(index)
    }
    if(index > 3 && index <=6){
      indexRel<-(index2)*3
      mBoxPlot2[,indexRel]<-timeEstimation
      mBoxPlot2[,indexRel-1]<-frequencyEstimation
      mBoxPlot2[,indexRel-2]<-weibletsEstimation
      index2=index2+1
    }
    if(index > 6)
    {
      indexRel<-(index3)*3
      mBoxPlot3[,indexRel]<-timeEstimation
      mBoxPlot3[,indexRel-1]<-frequencyEstimation
      mBoxPlot3[,indexRel-2]<-weibletsEstimation
      index3=index3+1
    }
    
    #h<-foo@hurst[1]
    
    #cbind(mBoxPlot,as.vector(timeEstimation))
    #cbind(mBoxPlot,as.vector(frequencyEstimation))
    #cbind(mBoxPlot,as.vector(weibletsEstimation))
    #print(cat("Para los valores H de " ,i,"\n"))
    #print("Estimación en el dominio del tiempo (t)\n")
    #print(cat("Media :",mean(timeEstimation)))
    #print(cat("Varianza :",var(timeEstimation)))
    #print(cat("MSE : ",mse(timeEstimation) ))
    
    index=index+1
  }
  
  
  #dev.off()
  #print(matriz)
  #print(df)
  
  #boxplot(data, las = 2, names = c("t 0.1","frq 0.1","wavelet 0.1","t 0.2","frq 0.2","wavelet 0.2","t 0.3","frq 0.3","wavelet 0.3","t 0.4","frq 0.4","wavelet 0.4"))
  dataTest<-data.frame(mBoxPlot)
  dataTest2<-data.frame(mBoxPlot2)
  dataTest3<-data.frame(mBoxPlot3)
  tableDF<-data.frame(matriz)
  boxplot(dataTest,col=(c("gold","green","blue")),names = c("t","f 0.1","wavelet","t","f 0.2","wavelet","t","f 0.3","wavelet") , main="Estimations with H 0.1 to 0.3", 
          xlab="Estimations (f,t,wavelet)", ylab="Aproximation")
  boxplot(dataTest2,col=(c("gold","green","blue")),names = c("t","f 0.4","wavelet","t","f 0.5","wavelet","t","f 0.6","wavelet") , main="Estimations with H 0.4 to 0.6", 
          xlab="Estimations (f,t,wavelet)", ylab="Aproximation")
  boxplot(dataTest3,col=(c("gold","green","blue")),names = c("t","f 0.7","wavelet","t","f 0.8","wavelet","t","f 0.9","wavelet") , main="Estimations with H 0.7 to 0.9", 
          xlab="Estimations (f,t,wavelet)", ylab="Aproximation")
  #boxplot(dataTest2)
  hgValues<-seq(0.1,0.9,by=0.1)
  colsValues<-c("Mean-t","Var-t","MSE-t","Mean-f","Var-f","MSE-f","Mean-w","Var-w","MSE-w")
  tbl <- tableGrob(tableDF,
                   rows = hgValues,
                   cols = colsValues)
  
  grid.arrange(tbl,
               nrow=1,
               as.table=TRUE,
               heights=c(4,1))
  #plot.table(matriz, format(as.Date(Sys.time()), '%d %b %Y'))
  print(tableDF)
  #print(dataTest)
}
################

