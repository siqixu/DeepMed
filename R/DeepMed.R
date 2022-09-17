DeepMed=function(y,d,m,x,method="DNN",hyper_grid=NA,epochs=500,batch_size=100,trim=0.05){

  if(method!="Lasso"){
    hyper_grid = as.data.frame(hyper_grid)
    hyper=DeepMed_cv(y,d,m,x,method,hyper_grid,epochs,batch_size)
  }else{
    hyper=matrix(NA,nrow=2,ncol=30)
  }

  mbin=1*(length(unique(m))==2 & min(m)==0 & max(m)==1)
  if (mbin==0) temp=DeepMed_cont(y,d,m,x,method,hyper,trim)
  if (mbin!=0) temp=DeepMed_bin(y,d,m,x,method,hyper,trim)
  ATE = temp$ATE

  eff=ATE[1:5]
  se=sqrt( (ATE[6:10])/ATE[11])
  results=rbind(eff,se, 2*pnorm(-abs(eff/se)))
  colnames(results)=c("total", "dir.treat", "dir.control", "indir.treat", "indir.control")
  rownames(results)=c("effect","se","pval")
  ntrimmed=length(d)-ATE[11]

  list("results"=results, "ntrimmed"=ntrimmed)
}


