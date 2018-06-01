################################################################################

#Calculates the maximum likelihood estimates of a logistic regression model

#
# fmla : model formula
# x : a [n x p] dataframe with the data. Factors should be coded accordingly
#
# OUTPUT
# beta : the estimated regression coefficients
# vcov : the variane-covariance matrix
# ll : -2ln L (deviance)
#
################################################################################
# Author : Thomas Debray
# Version : 22 dec 2011
################################################################################

mle.logreg = function(fmla, data)
{
# Define the negative log likelihood function
logl <- function(theta,x,y){
  y <- y
  x <- as.matrix(x)
  beta <- theta[1:ncol(x)]
  
  # Use the log-likelihood of the Bernouilli distribution, where p is
  # defined as the logistic transformation of a linear combination
  # of predictors, according to logit(p)=(x%*%beta)
  loglik <- sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
  return(-loglik)
}

# Prepare the data
outcome = rownames(attr(terms(fmla),"factors"))[1]
dfrTmp = model.frame(data)
x = as.matrix(model.matrix(fmla, data=dfrTmp))
y = as.numeric(as.matrix(data[,match(outcome,colnames(data))]))

# Define initial values for the parameters
theta.start = rep(0,(dim(x)[2]))
names(theta.start) = colnames(x)

# Calculate the maximum likelihood
mle = optim(theta.start,logl,x=x,y=y,hessian=T)
out = list(beta=mle$par,vcov=solve(mle$hessian),ll=2*mle$value)
}
################################################################################
#We can implement this function as follows:

mydata$rank = factor(mydata$rank) #Treat rank as a categorical variable
fmla = as.formula("admit~gre+gpa+rank") #Create model formula
mylogit = mle.logreg(fmla, mydata) #Estimate coefficients
mylogit
Note that the categorical variable rank is modeled as a factor. This implies that a separate regression coefficient is estimated for ranks 2, 3 and 4 (with rank 1 as reference). Instead of obtaining the observed information matrix from the numerically differentiated Hessian matrix (through the optim-command), it is possible to calculate an unbiased estimate directly from the data:
  
  ################################################################################
# Calculates the maximum likelihood estimates of a logistic regression model
#
# fmla : model formula
# x : a [n x p] dataframe with the data. Factors should be coded accordingly
#
# OUTPUT
# beta : the estimated regression coefficients
# vcov : the variane-covariance matrix
# ll : -2ln L (deviance)
#
################################################################################
# Author : Thomas Debray
# Version : 22 dec 2011
################################################################################
mle.logreg = function(fmla, data)
{
  # Define the negative log likelihood function
  logl <- function(theta,x,y){
    y <- y
    x <- as.matrix(x)
    beta <- theta[1:ncol(x)]
    
    # Use the log-likelihood of the Bernouilli distribution, where p is
    # defined as the logistic transformation of a linear combination
    # of predictors, according to logit(p)=(x%*%beta)
    loglik <- sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
    return(-loglik)
  }
  
  # Prepare the data
  outcome = rownames(attr(terms(fmla),"factors"))[1]
  dfrTmp = model.frame(data)
  x = as.matrix(model.matrix(fmla, data=dfrTmp))
  y = as.numeric(as.matrix(data[,match(outcome,colnames(data))]))
  
  # Define initial values for the parameters
  theta.start = rep(0,(dim(x)[2]))
  names(theta.start) = colnames(x)
  
  # Calculate the maximum likelihood
  mle = optim(theta.start,logl,x=x,y=y,hessian=F)
  
  # Obtain regression coefficients
  beta = mle$par
  
  # Calculate the Information matrix
  # The variance of a Bernouilli distribution is given by p(1-p)
  p = 1/(1+exp(-x%*%beta))
  V = array(0,dim=c(dim(x)[1],dim(x)[1]))
  diag(V) = p*(1-p)
  IB = t(x)%*%V%*%x
  
  # Return estimates
  out = list(beta=beta,vcov=solve(IB),dev=2*mle$value)
}
################################################################################
#Finally, in some scenarios it is necessary to constrain the parameter search space. For instance, in stacked regressions it is important to put non-negative constraints on the regression slopes. This can be achieved by a small modification in the optim-command:
  
  ################################################################################
# Calculates the maximum likelihood estimates of a logistic regression model
# Slopes are constrained to non-negative values
#
# fmla : model formula
# x : a [n x p] dataframe with the data. Factors should be coded accordingly
#
# OUTPUT
# beta : the estimated regression coefficients
# vcov : the variane-covariance matrix
# ll : -2ln L (deviance)
#
################################################################################
# Author : Thomas Debray
# Version : 22 dec 2011
################################################################################
mle.logreg.constrained = function(fmla, data)
{
  # Define the negative log likelihood function
  logl <- function(theta,x,y){
    y <- y
    x <- as.matrix(x)
    beta <- theta[1:ncol(x)]
    
    # Use the log-likelihood of the Bernouilli distribution, where p is
    # defined as the logistic transformation of a linear combination
    # of predictors, according to logit(p)=(x%*%beta)
    loglik <- sum(-y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
    return(-loglik)
  }
  
  # Prepare the data
  outcome = rownames(attr(terms(fmla),"factors"))[1]
  dfrTmp = model.frame(data)
  x = as.matrix(model.matrix(fmla, data=dfrTmp))
  y = as.numeric(as.matrix(data[,match(outcome,colnames(data))]))
  
  # Define initial values for the parameters
  theta.start = rep(0,(dim(x)[2]))
  names(theta.start) = colnames(x)
  
  # Non-negative slopes constraint
  lower = c(-Inf,rep(0,(length(theta.start)-1)))
  
  # Calculate the maximum likelihood
  mle = optim(theta.start,logl,x=x,y=y,hessian=T,lower=lower,method="L-BFGS-B")
  
  # Obtain regression coefficients
  beta = mle$par
  
  # Calculate the Information matrix
  # The variance of a Bernouilli distribution is given by p(1-p)
  p = 1/(1+exp(-x%*%beta))
  V = array(0,dim=c(dim(x)[1],dim(x)[1]))
  diag(V) = p*(1-p)
  IB = t(x)%*%V%*%x
  
  # Return estimates
  out = list(beta=beta,vcov=solve(IB),dev=2*mle$value)
}
################################################################################