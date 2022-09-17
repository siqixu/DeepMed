DeepMed_bin_cv=function(y,d,m,x,method,hyper_grid,epochs,batch_size){

  y=as.vector(y);d=as.vector(d);m=as.vector(m)
  if(is.null(nrow(x))){x=matrix(as.matrix(x),length(x),1)
  }else{x=matrix(as.matrix(x),nrow(x),ncol(x))}

  if(method=="DNN"){hyper_grid=cbind(hyper_grid,epochs,batch_size)}
  n_hyper=ncol(hyper_grid)

  stepsize=ceiling((1/3)*length(d))
  set.seed(1); idx= sample(length(d), replace=FALSE)
  # crossfitting procedure that splits sample in training an testing data
  hyper=numeric()
  for (k in 1:3){
    tesample=idx[((k-1)*stepsize+1):(min((k)*stepsize,length(d)))]
    dtr=d[-tesample]; dte=d[tesample]; ytr=y[-tesample]; yte=y[tesample]; ytr1=ytr[dtr==1]; ytr0=ytr[dtr==0];
    mtr=m[-tesample]; mte=m[tesample]; mtr1=mtr[dtr==1]; mtr0=mtr[dtr==0];
    if (is.null(ncol(x)) | ncol(x)==1) {
      xtr=x[-tesample]; xte=x[tesample]; xtr1=xtr[dtr==1]; xtr0=xtr[dtr==0]; xtr11=xtr[dtr==1 & mtr==1]; xtr10=xtr[dtr==1 & mtr==0]; xtr01=xtr[dtr==0 & mtr==1]; xtr00=xtr[dtr==0 & mtr==0]
    }
    if (is.null(ncol(x))==0 & ncol(x)>1) {
      xtr=x[-tesample,]; xte=x[tesample,]; xtr1=xtr[dtr==1,]; xtr0=xtr[dtr==0,]; xtr11=xtr[dtr==1 & mtr==1,]; xtr10=xtr[dtr==1 & mtr==0,]; xtr01=xtr[dtr==0 & mtr==1,]; xtr00=xtr[dtr==0 & mtr==0,]
    }
    ytr11=ytr[dtr==1 & mtr==1]; ytr10=ytr[dtr==1 & mtr==0]; ytr01=ytr[dtr==0 & mtr==1]; ytr00=ytr[dtr==0 & mtr==0];
    # tr stands for first training data, te for test data, "1" and "0" for subsamples with treated and nontreated

    hyper_k=numeric()
    # predict Pr(M=1|D=1,X) in test data
    # predict Pr(M=1|D=0,X) in test data
    # predict Pr(D=1|X) in test data
    # predict E(Y| D=1, M=1, X) in test data
    # predict E(Y| D=0, M=1, X) in test data
    # predict E(Y| D=1, M=0, X) in test data
    # predict E(Y| D=0, M=0, X) in test data
    # predict E(Y|D=1, X) in test data
    # predict E(Y|D=0, X) in test data
    out <- foreach(t=1:nrow(hyper_grid), .combine=rbind,.packages=c("keras","gbm","randomForest")) %dopar% {
      set.seed(1)
      out1 = ml_cv(mtr1, xtr1,method, hyper_grid, t)
      out2 = ml_cv(mtr0, xtr0,method, hyper_grid, t)
      out3 = ml_cv(dtr, xtr,method, hyper_grid, t)
      out4 = ml_cv(ytr11, xtr11,method, hyper_grid, t)
      out5 = ml_cv(ytr01, xtr01,method, hyper_grid, t)
      out6 = ml_cv(ytr10, xtr10,method, hyper_grid, t)
      out7 = ml_cv(ytr00, xtr00,method, hyper_grid, t)
      out8 = ml_cv(ytr1, xtr1,method, hyper_grid, t)
      out9 = ml_cv(ytr0, xtr0,method, hyper_grid, t)
      out = cbind(out1,out2,out3,out4,out5,out6,out7,out8,out9)
    }

    for(i in 1:9){
      outi=out[,1:(n_hyper+1)]
      out=out[,-c(1:(n_hyper+1))]
      loc = which.min(outi[,(n_hyper+1)])
      hyper_k = cbind(hyper_k, outi[loc,]  )
    }
    colnames(hyper_k)=1:9
    hyper_k[n_hyper+1,]=round(hyper_k[n_hyper+1,],3)
    if(method=="DNN"){hyper_k[4,]=round(hyper_k[4,])}

    if(k==1){hyper=hyper_k
    }else{hyper = cbind(hyper,hyper_k)}
  }

  return(hyper)
}
