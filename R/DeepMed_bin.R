DeepMed_bin=function(y,d,m,x,method,hyper=hyper,trim=0.05){

  if(method=="DNN"){ml=dnn}
  if(method=="GBM"){ml=gbm_out}
  if(method=="RF"){ml=rf_out}
  if(method=="Lasso"){ml=ls_out}

  stepsize=ceiling((1/3)*length(d))
  set.seed(1); idx= sample(length(d), replace=FALSE)
  y1m0=c();y1m1=c();y0m0=c(); y0m1=c(); selall=c()
  # crossfitting procedure that splits sample in training an testing data
  for (k in 1:3){
    tesample=idx[((k-1)*stepsize+1):(min((k)*stepsize,length(d)))]
    dtr=d[-tesample]; dte=d[tesample]; ytr=y[-tesample]; yte=y[tesample]; ytr1= ytr[dtr==1]; ytr0= ytr[dtr==0];
    mtr=m[-tesample]; mte=m[tesample]; mtr1=mtr[dtr==1]; mtr0=mtr[dtr==0];
    if (is.null(ncol(x)) | ncol(x)==1) {
      xtr=x[-tesample]; xte=x[tesample]; xtr1=xtr[dtr==1]; xtr0=xtr[dtr==0]; xtr11=xtr[dtr==1 & mtr==1]; xtr10=xtr[dtr==1 & mtr==0]; xtr01=xtr[dtr==0 & mtr==1]; xtr00=xtr[dtr==0 & mtr==0]
    }
    if (is.null(ncol(x))==0 & ncol(x)>1) {
      xtr=x[-tesample,]; xte=x[tesample,]; xtr1=xtr[dtr==1,]; xtr0=xtr[dtr==0,]; xtr11=xtr[dtr==1 & mtr==1,]; xtr10=xtr[dtr==1 & mtr==0,]; xtr01=xtr[dtr==0 & mtr==1,]; xtr00=xtr[dtr==0 & mtr==0,]
    }
    ytr11=ytr[dtr==1 & mtr==1]; ytr10=ytr[dtr==1 & mtr==0]; ytr01=ytr[dtr==0 & mtr==1]; ytr00=ytr[dtr==0 & mtr==0];
    # tr stands for first training data, te for test data, "1" and "0" for subsamples with treated and nontreated


    # predict Pr(M=1|D=1,X) in test data
    # predict Pr(M=1|D=0,X) in test data
    # predict Pr(D=1|X) in test data
    # predict E(Y| D=1, M=1, X) in test data
    # predict E(Y| D=0, M=1, X) in test data
    # predict E(Y| D=1, M=0, X) in test data
    # predict E(Y| D=0, M=0, X) in test data
    # predict E(Y|D=1, X) in test data
    # predict E(Y|D=0, X) in test data
    out <- foreach(t=1:9, .combine=cbind,.packages=c("keras","gbm","randomForest","hdm")) %dopar% {
      set.seed(1)
      if(t==1){pm1te = ml(mtr1,xtr1, mte,xte, hyper[,1])$ypred;return(pm1te)}
      if(t==2){pm0te = ml(mtr0,xtr0, mte,xte, hyper[,2])$ypred;return(pm0te)}
      if(t==3){pdte = ml(dtr, xtr, dte,xte, hyper[,3])$ypred;return(pdte)}
      if(t==4){eymx11te = ml(ytr11, xtr11, yte,xte, hyper[,4])$ypred;return(eymx11te)}
      if(t==5){eymx01te = ml(ytr01, xtr01, yte,xte, hyper[,5])$ypred;return(eymx01te)}
      if(t==6){eymx10te = ml(ytr10, xtr10, yte,xte, hyper[,6])$ypred;return(eymx10te)}
      if(t==7){eymx00te = ml(ytr00, xtr00, yte,xte, hyper[,7])$ypred;return(eymx00te)}
      if(t==8){eyx1te = ml(ytr1, xtr1, yte,xte, hyper[,8])$ypred;return(eyx1te)}
      if(t==9){eyx0te = ml(ytr0, xtr0, yte,xte, hyper[,9])$ypred;return(eyx0te)}
    }

    pm1te = out[,1]
    pm0te = out[,2]
    pdte = out[,3]
    eymx11te = out[,4]
    eymx01te = out[,5]
    eymx10te = out[,6]
    eymx00te = out[,7]
    eyx1te = out[,8]
    eyx0te = out[,9]
    hyper=hyper[,-c(1:9)]

    # predict E(Y| D=0, M, X) in test data
    eymx0te=mte*eymx01te+(1-mte)*eymx00te
    # predict E(Y| D=1, M, X) in test data
    eymx1te=mte*eymx11te+(1-mte)*eymx10te

    # predict score functions for E(Y(1,M(0))) in the test data
    eta10=(eymx11te*pm0te+eymx10te*(1-pm0te))
    sel= 1*(((pdte*pm1te)>=trim) & ((1-pdte)>=trim)  & (pdte>=trim) &  (((1-pdte)*pm0te)>=trim)   )
    temp=dte*pm0te/(pdte*pm1te)*(yte-eymx1te)+(1-dte)/(1-pdte)*(eymx1te-eta10)+eta10
    y1m0=c(y1m0, temp[sel==1])
    # predict score functions for E(Y(1,M(1))) in the test data
    temp=eyx1te + dte*(yte-eyx1te)/pdte
    y1m1=c(y1m1,temp[sel==1])
    # predict score functions for E(Y(0,M(1))) in the test data
    eta01=(eymx01te*pm1te+eymx00te*(1-pm1te))
    temp=(1-dte)*pm1te/((1-pdte)*pm0te)*(yte-eymx0te)+dte/pdte*(eymx0te-eta01)+eta01
    y0m1=c(y0m1, temp[sel==1])
    # predict score functions for E(Y0,M(0)) in the test data
    temp=eyx0te + (1-dte)*(yte-eyx0te)/(1-pdte)
    y0m0=c(y0m0, temp[sel==1])
    selall=c(selall,sel)
  }
  # average over the crossfitting steps
  my1m1=mean(y1m1); my0m1=mean(y0m1); my1m0=mean(y1m0); my0m0=mean(y0m0)
  # compute effects
  tot=my1m1-my0m0; dir1=my1m1-my0m1; dir0=my1m0-my0m0; indir1=my1m1-my1m0; indir0=my0m1-my0m0;
  #compute variances
  vtot=mean((y1m1-y0m0-tot)^2); vdir1=mean((y1m1-y0m1-dir1)^2); vdir0=mean((y1m0-y0m0-dir0)^2);
  vindir1=mean((y1m1-y1m0-indir1)^2); vindir0=mean((y0m1-y0m0-indir0)^2);

  ATE = c(tot, dir1, dir0, indir1, indir0, vtot, vdir1, vdir0, vindir1, vindir0, sum(selall))
  list("ATE"=ATE)
}






