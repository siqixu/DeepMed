
ml_cv=function(ytrain, xtrain,method, hyper_grid,t){

  if(method=="DNN"){ml=dnn}
  if(method=="GBM"){ml=gbm_out}
  if(method=="RF"){ml=rf_out}

  stepsize=ceiling((1/3)*length(ytrain))
  nobs = min(3*stepsize,length(ytrain)); set.seed(1); idx = sample(nobs);
  sample1 = idx[1:stepsize]; sample2 = idx[(stepsize+1):(2*stepsize)];
  sample3 = idx[(2*stepsize+1):nobs];

  loss=epoch_opt=numeric()
  for(i in 1:3){
    if (i==1) {tesample=sample1; trsample=c(sample2,sample3)}
    if (i==2) {tesample=sample3; trsample=c(sample1,sample2)}
    if (i==3) {tesample=sample2; trsample=c(sample1,sample3)}

    temp = ml(ytrain[trsample],xtrain[trsample,], ytrain[tesample],xtrain[tesample,],hyper_grid[t,])
    loss = c(loss, temp$loss)
    if(method=="DNN"){epoch_opt = c(epoch_opt,temp$epoch_opt)}
  }

  out=cbind(hyper_grid[t,],mean(loss))
  if(method=="DNN"){out[,4]=mean(epoch_opt)}
  out=as.matrix(out)
  return(out)
}




















