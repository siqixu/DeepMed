# DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning
DeepMed is an approach for semi-parametric causal mediation analysis to estimate the natural (in)direct effects of a binary treatment on an outcome of interest. DeepMed adopts the deep neural networks to estimate the nuisance parameters involved in the influence functions of the causal parameters.


## Setup
The "DeepMed" package requires R (>= 3.5.0), and depends on the R packages "keras", "tensorflow", "foreach", "gbm", "randomForest" and "hdm". 

Use the following command in R to install the "DeepMed" package:
```
library(devtools)
install_github("siqixu/DeepMed",ref="main") # install the "DeepMed" package

library(tensorflow)
install_tensorflow()  # install the "tensorflow" python package
```


## Usage
```
DeepMed(y,d,m,x,method="DNN",hyper_grid=NA,epochs=500,batch_size=100,trim=0.05)
```
`y`: A numeric vector for the outcome variable.

`d`: A numeric vector for the binary treatment variable, which is coded as 0 or 1.

`m`: A numeric vector or a numeric matrix for the mediator variable. When the mediator is categorical, m should be a matrix with q columns for q dummy variables. 

`x`: A numeric vector or a numeric matrix with p columns for p covariates.

`method`: The method used to estimate the nuisance functions with a 3-fold cross-fitting. Four methods are provided: deep neural network ("DNN""), gradient boosting machine ("GBM"), random forest ("RF") and Lasso ("Lasso"). See details below. By default, `method="DNN"`.

`hyper_grid`: A numeric matrix containing a grid of candidate hyperparameters for "DNN", "GBM", or "RF" (see details below). A 3-fold cross-validation is used to select the hyperparameters over `hyper_grid` based on the cross-entropy loss for binary response and the mean-squared loss for continuous response. If `method=="Lasso"`, this argument will be ignored.

`epochs`: The maximum number of candidate epochs in deep neural network. By default, `epochs=500`. If `method!="DNN"`, this argument will be ignored.

`batch_size`: The batch size of deep neural network. By default, `batch_size=100`. If `method!="DNN"`, this argument will be ignored.
  
`trim`: The trimming rate for preventing conditional treatment or mediator probabilities from being zero. Observations with any denominators in the potential outcomes smaller than the trimming rate will be excluded from the analysis. By default, `trim=0.05`.

## Value
`results`: The estimates (`effect`), standard errors (`se`) and P values (`pval`) of the total treatment effect (`total`), (in)direct treatment effect in treated (`(in)dir.treat`), and (in)direct treatment effect in control group (`(in)dir.control`).
 
`ntrimmed`: The number of observations being excluded due to the denominators in the potential outcomes smaller than the trimming rate. 


## Details
All binary variables in the data should be coded as 0 or 1.

Four methods are provided to estimate the nuisance functions. "DNN" is built up by using the "keras" R package. "GBM" and "RF" are implemented using the R packages “gbm” and “randomForest”, respectively. "Lasso" is implemented using the “hdm” R package with a data-driven penalty.

`hyper_grid` is a numeric matrix for the candidate hyperparameters of "DNN", "GBM", or "RF". If `method=="DNN"`, it has three columns for the L1 regularization parameter in the input layer, the number of hidden layers, and the number of hidden units, respectively. If `method=="GBM"`, it has two columns for the maximum depth of each tree and the total number of trees, respectively. If `method=="RF"`, it has two columns for the minimum size of terminal nodes and the number of trees, respectively. A 3-fold cross-validation is used to select the hyperparameters over `hyper_grid`. Other hyperparameters involved in these methods are set to be the default values in the corresponding R packages.


## References
Xu S, Liu L and Liu Z. DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning. NeurIPS 2022.


## Acknowledgement
Some functions in " DeepMed" package are built upon the framework of the "medDML" function from the R package "causalweight". We thank the authors, Hugo Bodory and Martin Huber, of the "causalweight" package.


## Examples
```
library(DeepMed)
# use parallel computation with 10 cores
library(doParallel);registerDoParallel(cores=10)

# DNN
l1 = c(0,0.05)   # the L1 regularizition parameter of the input layer
layer = c(1,2)   # the number of hidden layers
unit = c(10,20)  # the number of hidden units
hyper_grid = expand.grid(l1,layer,unit) # create a grid of candidate hyperparameters

# run DeepMed on the example data with 1000 observations and two covariates.
DeepMed(y,d,m,x,method="DNN",hyper_grid)  # The computation time is around 0.5 hour.

# GBM
depth = c(1:3)        # the maximum depth of each tree
trees = c(10,50,100)  # the total number of trees
hyper_grid = expand.grid(depth,trees)
DeepMed(y,d,m,x,method="GBM",hyper_grid)

# Random Forest
nodes = c(1:3)         # the minimum size of terminal nodes
trees = c(10,50,100)   # the number of trees
hyper_grid = expand.grid(nodes,trees)
DeepMed(y,d,m,x,method="RF",hyper_grid)

# Lasso
DeepMed(y,d,m,x,method="Lasso")
```

## Simulation with $\rm H\ddot{o}lder$ functions
```
# Use the following command in R to install the "DaubechiesSim" package:

library(devtools)
install_github("siqixu/DaubechiesSim",ref="main") 

# Example

library(wavethresh)
library(DaubechiesSim)
library(doMC); registerDoMC(50)

T = 200     # simulation replicates
n = 10000   # sample size
s = 5       # number of covariates 
beta = 1.5
Ry = 1
filter.number = 5
resolution = 15
gammas = c(0,3,6,9,10,16)

wv.mother <- draw(filter.number = filter.number, family = "DaubExPhase", resolution = 2^resolution, scaling.function = FALSE, plot.it = FALSE, enhance = FALSE)

simDat <- function(s, n, t, wv.mother, filter.number, Ry, gammas, beta){
  set.seed(t)  
  x=matrix(runif(n*s,-1,1),ncol=s) 
  h_x=matrix(wv_trans_fast(0.5*x, wv.mother, filter.number, Ry, gammas, beta),ncol=s) # Generate data from Holder function
  simX=cbind(x,h_x)
  return(simX)
}
out <- foreach(i=1:T,.packages=c("DaubechiesSim")) %dopar% {
   simDat(s, n, t=i, wv.mother, filter.number, Ry, gammas, beta)
}

```
  
   
