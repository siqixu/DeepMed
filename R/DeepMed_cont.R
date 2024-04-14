
DeepMed_cont=function(y,d,m,x,method,hyper,trim=0.05){

  xm=as.matrix(cbind(x,m))
  
  if(method=="DNN"){ml=dnn}
  if(method=="GBM"){ml=gbm_out}
  if(method=="RF"){ml=rf_out}
  if(method=="Lasso"){ml=ls_out}

  stepsize=ceiling((1/3)*length(d))
  nobs = min(3*stepsize,length(d)); set.seed(1); idx = sample(nobs);
  sample1 = idx[1:stepsize]; sample2 = idx[(stepsize+1):(2*stepsize)];
  sample3 = idx[(2*stepsize+1):nobs];
  y1m0=c();y1m1=c();y0m0=c(); y0m1=c(); selall=c()

  for (k in 1:3){
    if (k==1) {tesample=sample1; musample=sample2; deltasample=sample3}
    if (k==2) {tesample=sample3; musample=sample1; deltasample=sample2}
    if (k==3) {tesample=sample2; musample=sample3; deltasample=sample1}
    trsample=c(musample,deltasample); dte=d[tesample]; yte=y[tesample]
    dtrte=d[deltasample]; xtrte=x[deltasample,]


    out <- foreach(t=1:6, .combine=cbind,.packages=c("keras","gbm","randomForest","hdm")) %dopar% {
      set.seed(1)
      if(t==1){pmxte = ml(d[trsample],xm[trsample,],d[tesample],xm[tesample,], hyper[,1])$ypred;return(pmxte)}
      if(t==2){pxte  = ml(d[trsample],x[trsample,],d[tesample],x[tesample,],hyper[,2])$ypred;return(pxte)}
      if(t==3){
        eymx1te_all = ml(y[musample[d[musample]==1]],xm[musample[d[musample]==1],],
                         y[c(tesample,deltasample)],xm[c(tesample,deltasample),],hyper[,3])$ypred
        eymx1te = eymx1te_all[1:length(tesample)] # ypredict E(Y|M,X,D=1) in test data
        eymx1trte = eymx1te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=1) in delta sample
        regweymx1te = ml(eymx1trte[dtrte==0],xtrte[dtrte==0,],eymx1te, x[tesample,],hyper[,4])$ypred
        return(cbind(eymx1te,regweymx1te))
      }
      if(t==4){eyx1te = ml(y[trsample[d[trsample]==1]],x[trsample[d[trsample]==1],],y[tesample], x[tesample,], hyper[,5])$ypred;return(eyx1te)}
      if(t==5){
        eymx0te_all = ml(y[musample[d[musample]==0]],xm[musample[d[musample]==0],],
                         y[c(tesample,deltasample)],xm[c(tesample,deltasample),],hyper[,6])$ypred
        eymx0te = eymx0te_all[1:length(tesample)] # ypredict E(Y|M,X,D=0) in test data
        eymx0trte = eymx0te_all[-(1:length(tesample))]  # ypredict E(Y|M,X,D=0) in delta sample
        regweymx0te = ml(eymx0trte[dtrte==1],xtrte[dtrte==1,],eymx0te, x[tesample,], hyper[,7])$ypred
        return(cbind(eymx0te,regweymx0te))
      }
      if(t==6){eyx0te = ml(y[trsample[d[trsample]==0]],x[trsample[d[trsample]==0],],y[tesample], x[tesample,], hyper[,8])$ypred;return(eyx0te)}

    }
    pmxte=out[,1]
    pxte=out[,2]
    eymx1te=out[,3]
    regweymx1te=out[,4]
    eyx1te=out[,5]
    eymx0te=out[,6]
    regweymx0te=out[,7]
    eyx0te=out[,8]
    hyper=hyper[,-c(1:8)]

    # select observations satisfying trimming restriction
    sel= 1*((((1-pmxte)*pxte)>=trim) & ((1-pxte)>=trim)  & (pxte>=trim) &  (((pmxte*(1-pxte)))>=trim)   )
    # ypredict E(Y0,M(1)) in the test data
    temp=((1-dte)*pmxte/((1-pmxte)*pxte)*(yte-eymx0te)+dte/pxte*(eymx0te-regweymx0te)+regweymx0te)
    y0m1=c(y0m1,temp[sel==1])
    # ypredict E(Y0,M(0)) in the test data
    temp=(eyx0te + (1-dte)*(yte-eyx0te)/(1-pxte))
    y0m0=c(y0m0,temp[sel==1])
    # ypredict E(Y1,M(0)) in the test data
    temp=(dte*(1-pmxte)/(pmxte*(1-pxte))*(yte-eymx1te)+(1-dte)/(1-pxte)*(eymx1te-regweymx1te)+regweymx1te)
    y1m0=c(y1m0,temp[sel==1])
    # ypredict E(Y1,M(1)) in the test data
    temp=(eyx1te + dte*(yte-eyx1te)/pxte)
    y1m1=c(y1m1,temp[sel==1])
    # collect selection dummies
    selall=c(selall,sel)

  }
  # average over the crossfitting steps
  my1m1=mean(y1m1); my0m1=mean(y0m1); my1m0=mean(y1m0); my0m0=mean(y0m0)
  # compute effects
  tot=my1m1-my0m0; dir1=my1m1-my0m1; dir0=my1m0-my0m0; indir1=my1m1-my1m0; indir0=my0m1-my0m0;
  #compute variances
  vtot=mean((y1m1-y0m0-tot)^2); vdir1=mean((y1m1-y0m1-dir1)^2); vdir0=mean((y1m0-y0m0-dir0)^2);
  vindir1=mean((y1m1-y1m0-indir1)^2); vindir0=mean((y0m1-y0m0-indir0)^2);

  ATE = c(tot, dir1, dir0, indir1, indir0,  vtot, vdir1, vdir0, vindir1, vindir0, sum(selall))
  list("ATE"=ATE)
}

















































