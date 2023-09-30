################################################################################
####################### ASSIGNMENT 1 - CHIARA TORRI ############################
################################################################################


############################### QUESTION 1 #####################################

# Load the data and define response vector and covariates matrix
Dati <- read.table("crime.txt",header = T,sep=";")
y <- Dati$y
Dati$y <- NULL
X <- Dati
X=as.matrix(X)

# Log transformation of y
log_y=log(y)

### Compute the beta_hat ###

# Compute XtX
X=model.matrix(log_y~X)
XtX=t(X)%*%X
# Invert
XtXm1= solve(XtX)
# Apply the formula of the beta
beta_hat=XtXm1%*%t(X)%*%log_y


### 95% confidence intervals ###
n=length(y)
p=length(X[1,])

# Hat matrix
H=X%*%XtXm1%*%t(X)

# Fitted values and residuals
log_y_hat=H%*%log_y
e_hat=(diag(1,n)-H)%*%log_y

# RSS and sigma2_hat
RSS=sum((e_hat)^2)
sig2_hat=RSS/(n-p)

# Confidence intervals
SE=c()
l=c()
u=c()

for (j in 1:length(beta_hat)){
  SE[j]=sqrt(sig2_hat*(XtXm1[j,j]))
  l[j]=beta_hat[j]-qt(p=1-0.025, df=n-p)*SE[j]
  u[j]=beta_hat[j]+qt(p=1-0.025, df=n-p)*SE[j]
}

CI95=data.frame(l,u)


### Test and p-values ###

t=c()
p_val=c()

for (j in 1:length(beta_hat)){
  t[j]=beta_hat[j]/SE[j]
  p_val[j]=2*pt(-abs(t[j]), df=n-p)
}

test=data.frame(t,p_val)

OLS=data.frame(beta_hat,CI95,test)
OLS

### Parameters for which i cannot reject H0 ###
not_significant=data.frame(OLS[which(test$p_val>0.05),c(1,5)])
cat("The parameters for which I can not reject H0: beta_j=0:")
not_significant
dim(not_significant)


### Deviance decomposition ###
bar_log_y=mean(log_y)

TSS=sum((log_y-bar_log_y)^2)
ESS=sum((log_y_hat-bar_log_y)^2)
cat("TSS:",TSS,"ESS:",ESS,"RSS:",RSS)
cat("The sum of ESS and RSS is:", ESS+RSS) #equal to TSS

# R2
R2=ESS/TSS
cat("R2=",R2)




rm(list = ls())



############################### QUESTION 2 #####################################

Dati <- read.table("crime.txt",header = T,sep=";")
y <- Dati$y
Dati$y <- NULL
X <- Dati
log_y=log(y)

X<-as.matrix(scale(X))
Xcor=cor(X)

library(ggcorrplot)
ggcorrplot(Xcor, tl.cex=3, tl.srt=90)

rm(list = ls())


############################## QUESTION 3 ######################################

### NUMBER OF SUBMODELS ###

# There are 2^p models, so in this case
2^122


### STEPWISE SELECTION ###

Dati <- read.table("crime.txt",header = T,sep=";")
y <- Dati$y
Dati$y <- NULL
X <- Dati
X<-as.matrix(scale(X))
log_y=log(y)

Dati=data.frame(X,log_y)

## Forward
library(leaps)
regfit.fwd = regsubsets(log_y ~ . , data=Dati, method = "forward", nvmax=122)

reg_summary_fwd = summary(regfit.fwd)

plot(regfit.fwd, scale = "r2")
adjr2_max=which.max(reg_summary_fwd$adjr2) # intercept + 71 covariates
cat("The best model using the adjusted R2 criterion is the on with",
    adjr2_max,"covariates")

plot(regfit.fwd, scale = "bic") 
bic_min=which.min(reg_summary_fwd$bic) # model with intercept + 22 covariates
cat("The best model using the BIC criterion is the on with", bic_min,
    "covariates")

plot(regfit.fwd, scale = "Cp")
Cp_min=which.min(reg_summary_fwd$cp) # intercept + 51 covariates
cat("The best model using the Mallow's Cp criterion is the on with", 
    Cp_min,"covariates")

# Plots
x11()
par(mfrow = c(2,2))

# Adj R2
plot(reg_summary_fwd$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
abline(v=adjr2_max, col ="blue", lwd=2)

# BIC
plot(reg_summary_fwd$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
abline(v=bic_min, col ="blue", lwd=2)

# Cp
plot(reg_summary_fwd$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
abline(v=Cp_min, col ="blue", lwd=2)


# BIC coefficients
fwd_coeff=data.frame(coef(regfit.fwd, 22))
cat("The coefficients of the model selected using BIC are:")
fwd_coeff

#Adjusted R2 for question 4 comparison
cat("The adjusted R2 of the model is:", reg_summary_fwd$adjr2[22])




## Backward
regfit.back = regsubsets(log_y ~ . , data=Dati, method = "backward", nvmax=122)

reg_summary_back = summary(regfit.back)

par(mfrow=c(1,1))
plot(regfit.back, scale = "r2")
adjr2_max=which.max(reg_summary_back$adjr2) # intercept + 69 covariates
cat("The best model using the adjusted R2 criterion is the on with",
    adjr2_max,"covariates")

plot(regfit.back, scale = "bic") 
bic_min=which.min(reg_summary_back$bic) # intercept + 22 covariates
cat("The best model using the BIC criterion is the on with", bic_min,
    "covariates")

plot(regfit.back, scale = "Cp")
Cp_min=which.min(reg_summary_back$cp) # intercept + 46 covariates
cat("The best model using the Mallow's Cp criterion is the on with", 
    Cp_min,"covariates")

# Plots
x11()
par(mfrow = c(2,2))

# Adj R2
plot(reg_summary_back$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
abline(v=adjr2_max, col ="blue", lwd=2)

# BIC
plot(reg_summary_back$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
abline(v=bic_min, col ="blue", lwd=2)

# Cp
plot(reg_summary_back$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
abline(v=Cp_min, col ="blue", lwd=2)


# BIC coefficients
back_coeff=data.frame(coef(regfit.back, 22))
cat("The coefficients of the model selected using BIC are:")
back_coeff


#Adjusted R2 for question 4 comparison
cat("The adjusted R2 of the model is:",reg_summary_back$adjr2[22])


rm(list = ls())




### MY CODE FOR STEPWISE SELECTION ###

Dati <- read.table("crime.txt",header = T,sep=";")
y <- Dati$y
Dati$y <- NULL
X <- Dati
X<-as.matrix(scale(X))
log_y=log(y)

Dati=data.frame(X,log_y)


## Forward

p=length(X[1,])

# Available covariates= the ones that have not been inserted yet
var_names=colnames(X)
available=var_names


# First iteration: find M1
adj_r_k = c() # vector of the adjusted R2

# for all the models in Call_M1 (which are the ones in available)
for(j in 1:length(available)){
  
  # Select the row of Call_mk, which contains the columns of X 
  # to consider in the model
  X_j = subset(X, select = available[j])
  X_j=data.frame(X_j)
  
  # Compute adj R2
  adj_r_k[j] = summary(lm(log_y ~ ., X_j))$adj.r.squared
  
}

# Insert the variable that most ameliorates the model
M=c()
M[1]=available[which.max(adj_r_k)]

# Drop the best from available variables
available=available[!available==M]


# Next iterations: find M2,...,Mp
for (k in 2:p){
  
  # Define Call_Mk
  Mk_matr=matrix(rep(M, length(available)), nrow = length(available), byrow = T)
  Call_Mk= data.frame(Mk_matr,available)
  
  # Create the vector for the adjusted R2
  adj_r_k = c()
  
  # for all the models in Call_Mk
  for(j in 1:nrow(Call_Mk)){
    
    # Select the row of Call_mk, which contains the columns of X 
    # to consider in the model
    covar=unlist(Call_Mk[j,])
    X_j = subset(X, select = covar)
    X_j=data.frame(X_j)
    
    # Compute the adjusted R2 
    adj_r_k[j] = summary(lm(log_y ~ ., X_j))$adj.r.squared
    
  }
  
  # Insert the variable that most ameliorates the model
  M[k]=available[which.max(adj_r_k)]
  
  # Drop the best from available
  available=available[!available==M[k]]
}

# Now I have my M_1:p models

# Compute adjusted R2 and BIC for each Mk
bic=c()
adj_r2=c()
cp=c()
for (k in 1:length(M)){
  X_k=subset(X, select=M[1:k])
  X_k=data.frame(X_k)
  mod=lm(log_y ~., X_k)
  bic[k]=BIC(mod)
  adj_r2[k]=summary(mod)$adj.r.squared
}

cat("The best model using the adjusted R2 criterion is the on with",
    which.max(adj_r2),"covariates")

cat("The best model using the BIC criterion is the on with", which.min(bic),
    "covariates")


# Coefficients of the model chosen by BIC
X_k=subset(X, select=M[1:which.min(bic)])
X_k=data.frame(X_k)
mod=lm(log_y ~., X_k)
summary(mod)$coeff


rm(list = ls())







############################## QUESTION 4 ######################################

### Ridge ###

Dati <- read.table("crime.txt",header = T,sep=";")
y <- Dati$y
Dati$y <- NULL
X <- Dati
X=as.matrix(scale(X))
log_y=log(y)
cen_ly=log_y-mean(log_y) # centered to eliminate the intercept

# Beta coefficients
lambda=seq(0,250,by=0.5)
I=diag(x=1, nrow=122, ncol=122)
XtX=t(X)%*%X
n=length(log_y)

beta_ridge=data.frame()

for (i in 1:length(lambda)){
  XtX=t(X)%*%X
  W=(solve(XtX+lambda[i]*I))
  beta_ridge[1:122,i]=W%*%t(X)%*%cen_ly 
}


# Plot
plot(lambda,beta_ridge[1,], type="l", ylim=c(-5,5), 
     main="Standardized ridge regression \ncoefficients as function of lambda", 
     ylab="ridge coefficients")

for (i in 2:122){
  lines(lambda,beta_ridge[i,], type="l")
}

# Zoom in
plot(lambda,beta_ridge[1,], type="l", ylim=c(-0.3,0.3), 
     main="Standardized ridge regression \ncoefficients as function of lambda", 
     ylab="ridge coefficients")

for (i in 2:122){
  lines(lambda,beta_ridge[i,], type="l")
}


# GCV
GCV=c()
for (i in 1:length(lambda)){
  W=(solve(XtX+lambda[i]*I))
  H=X%*%W%*%t(X)
  cen_ly_hat=H%*%cen_ly
  tr_H=sum(diag(H))
  GCV[i]=1/n*sum(((cen_ly_hat-cen_ly)/(1-(tr_H)/n))^2)
}

opt_lambda=lambda[which.min(GCV)]
cat("The optimal lambda is:", opt_lambda)


# Estimation
beta_ridge_opt=beta_ridge[,which.min(GCV)]
cat("The coefficients of the ridge regression using the optimal lambda are:")
beta_ridge_opt

# Comparison with question 3 models
# Adjusted R2 of this model
W=(solve(XtX+opt_lambda*I))
H=X%*%W%*%t(X)
p=sum(diag(H))
cen_ly_hat=H%*%cen_ly
RSS=sum((cen_ly-cen_ly_hat)^2)
TSS=sum((cen_ly-mean(cen_ly))^2)
adj_r2=1-(RSS*(n-p-1))/(TSS*(n-1))
cat("The adjusted R2 of the model is:", adj_r2)


### Lasso ###

library(glmnet)
lasso_fit=glmnet(X, cen_ly)

# Plot with lambda
lasso_coef=as.matrix(coef(lasso_fit))
plot(lasso_fit$lambda, lasso_coef[1,], type="l", ylim=c(-0.4,0.4), 
     main="Standardized lasso regression \ncoefficients as function of lambda", 
     ylab="lasso coefficients", xlab="lambda")

for (i in 2:122){
  lines(lasso_fit$lambda,lasso_coef[i,], type="l")
}

# Plot with log lambda
plot(lasso_fit, xvar="lambda") 


# Optimal lambda
set.seed(123)
cv_lasso=cv.glmnet(X, cen_ly)

cv_lasso$lambda.min

plot(cv_lasso)
log(cv_lasso$lambda.min) # as shown in the graph



# Estimation
cat("The coefficients of the lasso regression with optimal lambda are:")
coef(cv_lasso, s = "lambda.min")
cat("Number of non-zero coefficients:", sum((coef(cv_lasso, s = "lambda.min"))!=0))

# Comparison with question 3 models
# Adjusted R2 of this model
p=sum((coef(cv_lasso, s = "lambda.min"))!=0) # number of non-zero coeff
cen_ly_hat= predict(cv_lasso, X, s="lambda.min")
RSS=sum((cen_ly-cen_ly_hat)^2)
TSS=sum((cen_ly-mean(cen_ly))^2)
adj_r2=1-(RSS*(n-p-1))/(TSS*(n-1))
cat("The adjusted R2 of the model is:", adj_r2)

