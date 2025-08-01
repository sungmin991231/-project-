runrf=function(Y,indice,horizon,selected){
#  comp=princomp(scale(Y,scale=FALSE))
#  Y2=cbind(Y,comp$scores[,1:4])
  Y2=Y
  
    
  X=embed(as.matrix(Y2),4)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  #Xout=t(as.vector(Xout))  # 이게 필요하나?
  
  y=tail(Y2[,1],nrow(Xin))
  X = Xin 
  X.out = Xout
  ##

  set.seed(42)
  model=randomForest(X[,selected],y,importance=TRUE)
  pred=predict(model,X.out[selected])
    
  return(list("model"=model,"pred"=pred))
}


rf.rolling.window=function(Y,npred,indice=1,horizon=1,selected){
  
  save.importance=list()
   save.pred=matrix(NA,npred-horizon+1,1)
  for(i in npred:horizon){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    lasso=runrf(Y.window,indice,horizon,selected)
    save.pred[(1+npred-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+npred-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-npred+horizon-1),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,npred-horizon+1)-save.pred)^2))
  mae=mean(abs(tail(real,npred-horizon+1)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance))
}
