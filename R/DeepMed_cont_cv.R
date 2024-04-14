DeepMed_cont_cv=function(y,d,m,x,method,hyper_grid,epochs,batch_size){

  xm=as.matrix(cbind(x,m))
  
  if(method=="DNN"){ml=dnn}
  if(method=="GBM"){ml=gbm_out}
  if(method=="RF"){ml=rf_out}
  if(method=="Lasso"){ml=ls_out}

  if(method=="DNN"){hyper_grid=cbind(hyper_grid,epochs,batch_size)}
  n_hyper=ncol(hyper_grid)

  stepsize=ceiling((1/3)*length(d))
  nobs = min(3*stepsize,length(d)); set.seed(1); idx = sample(nobs);
  sample1 = idx[1:stepsize]; sample2 = idx[(stepsize+1):(2*stepsize)];
  sample3 = idx[(2*stepsize+1):nobs];

  # crossfitting procedure that splits sample in training an testing data
  hyper=numeric()
  for (k in 1:3){
    if (k==1) {tesample=sample1; musample=sample2; deltasample=sample3}
    if (k==2) {tesample=sample3; musample=sample1; deltasample=sample2}
    if (k==3) {tesample=sample2; musample=sample3; deltasample=sample1}

    trsample=c(musample,deltasample); dte=d[tesample]; yte=y[tesample]
    hyper_k=numeric()

    # 1. fit Pr(D=1|M,X) in total of training data
    # 2. fit Pr(D=1|X) in total of training data
    # 3. fit E(Y|M,X,D=1) in first training data
    # 5. predict E(Y|X,D=1) in the test data
    # 6. fit E(Y|M,X,D=0) in first training data
    # 8. predict E(Y|X,D=0) in the test data
    out <- foreach(t=1:nrow(hyper_grid), .combine=rbind,.packages=c("keras","gbm","randomForest")) %dopar% {
      set.seed(1)
      out1 = ml_cv(d[trsample],xm[trsample,],method, hyper_grid, t)
      out2 = ml_cv(d[trsample],x[trsample,],method, hyper_grid, t)
      out3 = ml_cv(y[musample[d[musample]==1]],xm[musample[d[musample]==1],],method, hyper_grid, t)
      out5 = ml_cv(y[trsample[d[trsample]==1]],x[trsample[d[trsample]==1],],method, hyper_grid, t)
      out6 = ml_cv(y[musample[d[musample]==0]],xm[musample[d[musample]==0],],method, hyper_grid, t)
      out8 = ml_cv(y[trsample[d[trsample]==0]],x[trsample[d[trsample]==0],],method, hyper_grid, t)
      out = cbind(out1,out2,out3,out5,out6,out8)
    }

    for(i in 1:6){
      outi=out[,1:(n_hyper+1)]
      out=out[,-c(1:(n_hyper+1))]
      loc=which.min(outi[,n_hyper+1])
      hyper_k=cbind(hyper_k, outi[loc,] )
    }
    colnames(hyper_k)=c(1,2,3,5,6,8)


    out <- foreach(t=1:nrow(hyper_grid), .combine=rbind,.packages=c("keras","gbm","randomForest")) %dopar% {
      set.seed(1)
      dtrte=d[deltasample]; xtrte=x[deltasample,]

      ############## 4. fit E[E(Y|M,X,D=1)|D=0,X] in delta sample
      eymx1te_all = ml(y[musample[d[musample]==1]],xm[musample[d[musample]==1],],
                             y[c(tesample,deltasample)],xm[c(tesample,deltasample),],
                             hyper_k[,"3"])$ypred
      eymx1te = eymx1te_all[1:length(tesample)] # ypredict E(Y|M,X,D=1) in test data
      eymx1trte = eymx1te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=1) in delta sample

      out4 = ml_cv(eymx1trte[dtrte==0],xtrte[dtrte==0,],method, hyper_grid, t)


      ############ 7. fit E[E(Y|M,X,D=0)|D=1,X] in delta sample

      eymx0te_all = ml(y[musample[d[musample]==0]],xm[musample[d[musample]==0],],
                             y[c(tesample,deltasample)],xm[c(tesample,deltasample),],
                             hyper_k[,"6"] )$ypred
      eymx0te = eymx0te_all[1:length(tesample)] # ypredict E(Y|M,X,D=0) in test data
      eymx0trte = eymx0te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=0) in delta sample

      out7 = ml_cv(eymx0trte[dtrte==1],xtrte[dtrte==1,], method,hyper_grid, t)
      out = cbind(out4,out7)
    }

    for(i in 1:2){
      outi=out[,1:(n_hyper+1)]
      out=out[,-c(1:(n_hyper+1))]
      loc=which.min(outi[,n_hyper+1])
      hyper_k=cbind(hyper_k, outi[loc,]  )
    }
    colnames(hyper_k)=c(1,2,3,5,6,8,4,7)
    hyper_k=hyper_k[,c("1","2","3","4","5","6","7","8")]
    hyper_k[n_hyper+1,]=round(hyper_k[n_hyper+1,],3)
    if(method=="DNN"){hyper_k[4,]=round(hyper_k[4,])}

    if(k==1){hyper=hyper_k
    }else{hyper=cbind(hyper,hyper_k)}
  }

  return(hyper)
}


























