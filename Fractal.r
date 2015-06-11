#Declaracion de librerias
library('fractal')
library('fArma')
library('ggplot2')

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
  r<-FDWhittle(x, method="continuous", dc=FALSE, freq.max=0.5,
            delta.min=-1,delta.max=2.5, sdf.method="direct")
  unname(r)
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
  mBoxPlot<-matrix(0,9,3)
  df <- data.frame(Foo=NULL)
  #pdf('final2.pdf')
  index=1
  for(i in hgValues){
    tmp<- generateSignals(i,n,"fgn")
    for(j in 1:n)
    {
      timeEstimation[j]<-getTimeEstimation(tmp[[j]])[1]
      frequencyEstimation[j]<-getFrequencyEstimation(tmp[[j]])[1]
      weibletsEstimation[j]<-getTimeScaleEstimation(tmp[[j]])[1]
    }
    fr<-paste("Name", i, sep="")
    #print(fr)
    #df[fr]<- NA
    #df$Foo<-timeEstimation
    matriz[index,1] = mean(timeEstimation)
    matriz[index,2] = var(timeEstimation)
    matriz[index,3] = mse(timeEstimation)
    
    matriz[index,4] = mean(frequencyEstimation)
    matriz[index,5] = var(frequencyEstimation)
    matriz[index,6] = mse(frequencyEstimation)
    
    matriz[index,7] = mean(weibletsEstimation)
    matriz[index,8] = var(weibletsEstimation)
    matriz[index,9] = mse(weibletsEstimation)
    
   
    
    #print(cat("Para los valores H de " ,i,"\n"))
    #print("Estimación en el dominio del tiempo (t)\n")
    #print(cat("Media :",mean(timeEstimation)))
    #print(cat("Varianza :",var(timeEstimation)))
    #print(cat("MSE : ",mse(timeEstimation) ))
    
    index=index+1
  }
  
  
  #dev.off()
  print(matriz)
  print(df)
  data<-data.frame(Stat11=rnorm(100,mean=3,sd=2),
                   Stat21=rnorm(100,mean=4,sd=1),
                   Stat31=rnorm(100,mean=6,sd=0.5),
                   Stat41=rnorm(100,mean=10,sd=0.5),
                   Stat12=rnorm(100,mean=4,sd=2),
                   Stat22=rnorm(100,mean=4.5,sd=2),
                   Stat32=rnorm(100,mean=7,sd=0.5),
                   Stat42=rnorm(100,mean=8,sd=3),
                   Stat13=rnorm(100,mean=6,sd=0.5),
                   Stat23=rnorm(100,mean=5,sd=3),
                   Stat33=rnorm(100,mean=8,sd=0.2),
                   Stat43=rnorm(100,mean=4,sd=4))
  boxplot(data, las = 2, names = c("t 0.1","frq 0.1","wavelet 0.1","t 0.2","frq 0.2","wavelet 0.2","t 0.3","frq 0.3","wavelet 0.3","t 0.4","frq 0.4","wavelet 0.4"))
  
}
################

