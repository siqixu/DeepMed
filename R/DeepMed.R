DeepMed=function(y,d,m,x,method="DNN",hyper_grid=NA,epochs=500,batch_size=100,trim=0.05){

  y=as.vector(y); d=as.vector(d) 
  n=length(y)
  if(length(d)!=n){
    warning("The outcome variable and the treatment variable do not have the same sample size")
    break
  }
  
  if(is.vector(m)){ 
    if(length(m)!=n){
      warning("The outcome variable and the mediator variable do not have the same sample size")
      break
    }
  }else if(is.matrix(m)){
     if(nrow(m)!=n){
      warning("The outcome variable and the mediator variable do not have the same sample size")
      break
    }
  }else{
    warning("The mediator variable should be either a numeric vector or matrix")
    break
  }
    
  if(is.vector(x)){
    if(length(x)!=n){
      warning("The outcome variable and the covariate(s) do not have the same sample size")
      break
    }
  }else if(is.matrix(x)){
     if(nrow(x)!=n){
      warning("The outcome variable and the covariate(s) do not have the same sample size")
      break
    }
  }else{
    warning("The covariate(s) should be either a numeric vector or matrix")
    break
  }

    
  if(method!="Lasso"){
    hyper_grid=as.data.frame(hyper_grid)
    hyper=DeepMed_cv(y,d,m,x,method,hyper_grid,epochs,batch_size)
  }else{
    hyper=matrix(NA,nrow=2,ncol=30)
  }

 if(is.vector(m)){
   mbin=1*(length(unique(m))==2 & min(m)==0 & max(m)==1)
   if (mbin==0){temp=DeepMed_cont(y,d,m,x,method,hyper,trim)}
   if (mbin==1){temp=DeepMed_bin(y,d,m,x,method,hyper,trim)}
 }
  if(is.matrix(m)){temp=DeepMed_cont(y,d,m,x,method,hyper,trim)}
  
 
  ATE=temp$ATE

  eff=ATE[1:5]
  se=sqrt( (ATE[6:10])/ATE[11])
  results=rbind(eff,se, 2*pnorm(-abs(eff/se)))
  colnames(results)=c("total", "dir.treat", "dir.control", "indir.treat", "indir.control")
  rownames(results)=c("effect","se","pval")
  ntrimmed=length(d)-ATE[11]

  list("results"=results, "ntrimmed"=ntrimmed)
}


