\name{DeepMed}
\alias{DeepMed}
\title{DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning}
\usage{
DeepMed(y,d,m,x,method="DNN",hyper_grid=NA,epochs=500,batch_size=100,trim=0.05)
}
\description{Debiased Deep Learning for Semiparametric Causal Mediation Analysis
}

\arguments{
  \item{y}{A numeric vector for the outcome variable in causal mediation analysis.}
  \item{d}{A numeric vector for the binary treatment variable in causal mediation analysis, which is coded as 0 or 1.}
  \item{m}{A numeric vector for the mediator variable in causal mediation analysis.}
  \item{x}{A numeric vector or a numeric matrix with p columns for p covariates in causal mediation analysis.}
  \item{method}{The method used to estimate the nuisance functions with a 3-fold cross-fitting. Four methods are provided: deep neural network ("DNN""), gradient boosting machine ("GBM"), random forest ("RF") and Lasso ("Lasso"). See details below.  By default, \code{method="DNN"}.}
  \item{hyper_grid}{A numeric matrix containing a grid of candidate hyperparameters for "DNN", "GBM", or "RF" (see details below). A 3-fold cross-validation is used to select the hyperparameters over \code{hyper_grid} based on the cross-entropy loss for  binary response and the mean-squared loss for continuous response. If \code{method=="Lasso"}, this argument will be ignored.}
  \item{epochs}{The maximum number of candidate epochs in deep neural network. By default, \code{epochs=500}. If \code{method!="DNN"}, this argument will be ignored.}
  \item{batch_size}{The batch size of deep neural network. By default, batch_size=100. If \code{method!="DNN"}, this argument will be ignored.}
  \item{trim}{The trimming rate for preventing conditional treatment or mediator probabilities from being zero. Observations with any denominators in the potential outcomes smaller than the trimming rate will be excluded from the analysis. By default, \code{trim=0.05}.}
}

\value{
  \item{results}{The estimates (\emph{effect}), standard errors (\emph{se}) and P values (\emph{pval}) of the total treatment effect (\emph{total}), (in)direct treatment effect in treated (\emph{(in)dir.treat}), and (in)direct treatment effect in control group (\emph{(in)dir.control}).}
  \item{ntrimmed}{The number of observations being excluded due to the denominators in the potential outcomes smaller than the trimming rate.}
}

\details{All binary variables in the data should be coded as 0 or 1.

Four methods are provided to estimate the nuisance functions. "DNN" is built up by using the "keras" R package. "GBM" and "RF" are implemented using the R packages “gbm” and “randomForest”, respectively. "Lasso" is implemented using the “hdm” R package with a data-driven penalty.

\code{hyper_grid} is a numeric matrix for the candidate hyperparameters of "DNN", "GBM", or "RF". If \code{method=="DNN"}, it has three columns for the L1 regularization parameter in the input layer, the number of hidden layers, and the number of hidden units, respectively. If \code{method=="GBM"}, it has two columns for the maximum depth of each tree and the total number of trees, respectively. If \code{method=="RF"}, it has two columns for the minimum size of terminal nodes and the number of trees, respectively. A 3-fold cross-validation is used to select the hyperparameters over \code{hyper_grid}. Other hyperparameters involved in these methods are set to be the default values in the corresponding R packages.
}

\references{
Xu S, Liu L and Liu Z. DeepMed: Semiparametric Causal Mediation Analysis with Debiased Deep Learning. NeurIPS 2022.
}

\examples{
library(DeepMed)
# use parallel computation with 10 cores
library(doParallel);cl=makeCluster(10);registerDoParallel(cl)

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

}



