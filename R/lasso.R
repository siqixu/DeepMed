# lasso
ls_out=function(y, x, ytest, xtest,hyper=NA){
  colnames(xtest)=colnames(x)=NULL
  train.ybin=1*(length(unique(y))==2 & min(y)==0 & max(y)==1)

  if(train.ybin==1){
    temp <- rlassologit(y~x)
    ypred <- predict(temp, xtest, type="response")
    loss = -mean(ytest*log(ypred) + (1-ytest)*log(1-ypred))
  }else{
    temp <- rlasso(y~x)
    ypred <- predict(temp, xtest)
    loss =  mean((ypred-ytest)^2)
  }
  list("loss"=loss,"ypred"=ypred)
}
