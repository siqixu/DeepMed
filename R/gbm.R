#  gradient boosting machine

gbm_out = function(y, x, ytest, xtest, hyper){
  hyper = as.vector(hyper)
  dat = as.data.frame(cbind(y,x))
  datnew = as.data.frame(cbind(ytest,xtest))
  colnames(datnew)=colnames(dat)

  depth=hyper[1]
  ntree=hyper[2]

  train.ybin=1*(length(unique(y))==2 & min(y)==0 & max(y)==1)
  if(train.ybin==1){distribution = "bernoulli";loss_fun=function(ypred,ytest){-mean (ytest*log(ypred) + (1-ytest)*log(1-ypred))}}
  if(train.ybin!=1){distribution = "gaussian";loss_fun=function(ypred,ytest){mean((ypred-ytest)^2)}}

  temp = gbm(y~., data = dat,
             distribution = distribution, n.trees = ntree, shrinkage = 0.1,
             interaction.depth = depth, cv.folds = 1)
  ypred = predict(temp, newdata = datnew, n.trees = ntree, type = "response")
  loss = loss_fun(ypred,ytest)
  list("loss"=loss,"ypred"=ypred)
}
