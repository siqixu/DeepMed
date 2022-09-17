# random forest
rf_out = function(y, x, ytest, xtest, hyper){
  colnames(xtest)=colnames(x)=NULL
  train.ybin=1*(length(unique(y))==2 & min(y)==0 & max(y)==1)

  hyper=as.matrix(hyper)
  hyper=matrix(hyper,nrow=1)

  node=hyper[1]
  ntree=hyper[2]

  if(train.ybin==1){
    temp <- randomForest(x=x, y=as.factor(y), ntree=ntree, nodesize = node)
    ypred <- predict(temp, xtest, type="prob")[,2]
    ypred[ypred==1]=1-1e-5; ypred[ypred==0]=1e-5
    loss = -mean(ytest*log(ypred) + (1-ytest)*log(1-ypred))
  }else{
    temp <- randomForest(x=x, y=y, ntree=ntree, nodesize = node)
    ypred <- predict(temp, xtest)
    loss =  mean((ypred-ytest)^2)
  }
  list("loss"=loss,"ypred"=ypred)
}

