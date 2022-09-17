# built up DNN

dnn = function(y, x, ytest, xtest, hyper){
  train.ybin=1*(length(unique(y))==2 & min(y)==0 & max(y)==1)
  if(is.vector(x)){x = as.matrix(x)}

  hyper=as.matrix(hyper)
  hyper=matrix(hyper,nrow=1)
  l1=hyper[1]
  layers=hyper[2]
  units=hyper[3]
  epochs=hyper[4]
  batch_size=hyper[5]

  model <- keras_model_sequential()
  i = 0
  while(i<layers){
    if (i==0){ n_input = ncol(x); lambda=l1; units_i=  units
    }else{ n_input = units; lambda=0; units_i=units  }

    model %>% layer_dense(units = units_i, activation = 'relu', input_shape = n_input,
                          kernel_regularizer = regularizer_l1(lambda),
                          kernel_initializer = initializer_glorot_uniform(1))
    model %>% layer_batch_normalization()
    i = i+1
  }
  if(train.ybin==1){
    model %>% layer_dense(units=1, activation="sigmoid",kernel_initializer = initializer_glorot_uniform(1))
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer =  optimizer_adam(),
      metrics = "binary_crossentropy"
    )
  }else{
    model %>% layer_dense(units = 1,kernel_initializer = initializer_glorot_uniform(1) )
    model %>% compile(
      loss = "mean_squared_error",
      optimizer =  optimizer_adam(),
      metrics = "mean_squared_error"
    )
  }

  NNfit <- model %>% fit(x, y, epochs = epochs, batch_size=batch_size, verbose = 0,
                         validation_data = list (xtest, ytest))
  w=get_weights(model)[[1]]
  w=rowMeans(abs(w))

  ypred = model %>% predict(xtest)
  val_loss_all = NNfit[["metrics"]][["val_loss"]]
  loss = min(val_loss_all)
  epoch_opt = which.min(val_loss_all)

  list("loss"=loss,"epoch_opt"=epoch_opt,"ypred"=ypred)
}

















































































