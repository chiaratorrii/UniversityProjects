################################################################################
####################### ASSIGNMENT 2 - CHIARA TORRI ############################
################################################################################



############################# MODEL 1 ##########################################


### Case 1 ###


# Data
y=c(rep(0,16), rep(1,11))
n=length(y)
M=sum(y)
cat("The sample size is",n,"and the students that sleep more than 8 hours are",M)


# Hyper parameters
# E(theta)= alpha/(alpha+beta) = 0.3
# How sure we are: alpha+beta = ?
# To find the beta I choose the hyperparameters such that the 85th quantile of the
# Beta is approximately 0.5

alpha= 0.3*5.55
beta= 5.55-alpha
cat("alpha=",alpha,"beta=",beta)

qbeta(0.85, alpha, beta)
pbeta(0.5,alpha,beta)

# Posterior Hyper parameters
alpha_n= alpha+M
beta_n= beta+n-M
cat("alpha_n=",alpha_n,"beta_n=",beta_n)


# Plot of the prior
# The prior is the density of a Beta with, as parameters, the hyperparameters
curve(dbeta(x, alpha, beta), from=0, to=1, col="green", lwd=2, 
      xlab="theta", ylab="density", ylim=c(0,5))

# Plot of the likelihood
# The likelihood looks like the kernel of a Beta(M+1,n-M+1), therefore plotting 
#the density of a Beta with these parameters is equivalent to plot the normalized 
#version of the likelihood.
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=2, add=T)

# Plot of the posterior
# The posterior is the density of a Beta with updated parameters
curve(dbeta(x, alpha_n, beta_n), from=0, to=1, col="blue", lwd=2, add=T)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Note: the posterior is between the prior and the likelihood.


# Posterior expected values: the mean of a Beta is: alpha/(alpha+beta)
# The posterior mean is between the prior and the sample mean
prior_mean=alpha/(alpha+beta)
post_mean= alpha_n/(alpha_n+beta_n)
sample_avg= mean(y)
cat("Prior mean:", prior_mean,"Posterior mean:",post_mean,"Sample average:",sample_avg)


# Posterior variance: the variance of a Beta is: (alpha*beta)/(alpha+beta+1)*(alpha+beta)^2
prior_var= (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
post_var=(alpha_n*beta_n)/((alpha_n+beta_n+1)*(alpha_n+beta_n)^2)
cat("Prior variance:", prior_var,"Posterior variance:",post_var)


# Prior 95% credible interval
l0= qbeta(0.025, alpha, beta)
u0= qbeta(0.975, alpha, beta)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n= qbeta(0.025, alpha_n, beta_n)
u_n= qbeta(0.975, alpha_n, beta_n)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 85th quantiles
prior_85q=qbeta(0.85,alpha,beta)
post_85q=qbeta(0.85,alpha_n,beta_n)
cat("Prior 85th quantile:", prior_85q,", Posterior 85th quantile:",post_85q)


# Prediction

# The posterior predictive for a Bernoulli-Beta Model is: 
post_predictive=(alpha+M)/(alpha+beta+n)
cat("The posterior predictive is", post_predictive)
cat("The posterior predictive for the 20 observations of the new sample is", post_predictive*20)
# about 8 students over 20

# Example of the new sample
set.seed(123)
y_new= rbinom(20,1,post_mean)
cat("The new sample is", y_new)

cat("Students of the sample that sleep at least 8 hours:",sum(y_new))



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= pbeta(0.5,alpha_n,beta_n)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1=1-pbeta(0.5,alpha_n,beta_n)

# Pr(E0)
prior_prob0=pbeta(0.5,alpha,beta)

# Pr(E1)
prior_prob1=1-pbeta(0.5,alpha,beta)

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# "not worth more than bare mentioning"




### Case 2 ###

# Hyper parameters
# In the case of complete ignorance, I choose:
# E(theta)= alpha/(alpha+beta) = 0.5, because I place the same probability on 0 and 1
# alpha+beta = 0.01, because I'm very unsure of my choice

alpha= 0.5*0.01
beta= 0.01-alpha
cat("alpha=",alpha,"beta=",beta)

qbeta(0.85, alpha, beta)
pbeta(0.5,alpha,beta)


# Posterior Hyper parameters
alpha_n= alpha+M
beta_n= beta+n-M
cat("alpha_n=",alpha_n,"beta_n=",beta_n)


# Plot the prior
# The prior is the density of a Beta with parameters the hyperparameters
curve(dbeta(x, alpha, beta), from=0, to=1, col="green", lwd=2, 
      xlab="theta", ylab="density", ylim=c(0,5))

# Plot the likelihood
# The likelihood looks like the kernel of a Beta(M+1,n-M+1), therefore plotting 
#the density of a Beta with these parameters is equivalent to plot the normalized 
#version of the likelihood.
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=2, add=T)

# Plot the posterior
# The posterior is the density of a Beta with updated parameters
curve(dbeta(x, alpha_n, beta_n), from=0, to=1, col="blue", lwd=2, add=T)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Note: the posterior is more similar to the likelihood


# Posterior expected values: the mean of a Beta is: alpha/(alpha+beta)
# The posterior mean is between the prior and the sample mean
prior_mean=alpha/(alpha+beta)
post_mean= alpha_n/(alpha_n+beta_n)
sample_avg= mean(y)
cat("Prior mean:", prior_mean,"Posterior mean:",post_mean,"Sample average:",sample_avg)


# Posterior variance: the variance of a Beta is: (alpha*beta)/(alpha+beta+1)*(alpha+beta)^2
prior_var= (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
post_var=(alpha_n*beta_n)/((alpha_n+beta_n+1)*(alpha_n+beta_n)^2)
cat("Prior variance:", prior_var,"Posterior variance:",post_var)


# Prior 95% credible interval
l0= qbeta(0.025, alpha, beta)
u0= qbeta(0.975, alpha, beta)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n= qbeta(0.025, alpha_n, beta_n)
u_n= qbeta(0.975, alpha_n, beta_n)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 85th quantiles
prior_85q=qbeta(0.85,alpha,beta)
post_85q=qbeta(0.85,alpha_n,beta_n)
cat("Prior 85th quantile:", prior_85q,", Posterior 85th quantile:",post_85q)


# Prediction

# The posterior predictive for a Bernoulli-Beta Model is: 
post_predictive=(alpha+M)/(alpha+beta+n)
cat("The posterior predictive is", post_predictive)
cat("The posterior predictive for the 20 observations of the new sample is", post_predictive*20)
# about 8 students over 20

# Example of the new sample
set.seed(123)
y_new= rbinom(20,1,post_mean)
cat("The new sample is", y_new)

cat("Students of the sample that sleep at least 8 hours:",sum(y_new))



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= pbeta(0.5,alpha_n,beta_n)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1=1-pbeta(0.5,alpha_n,beta_n)

# Pr(E0)
prior_prob0=pbeta(0.5,alpha,beta)

# Pr(E1)
prior_prob1=1-pbeta(0.5,alpha,beta)

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# substantial





### Case 3 ###


# Prior hyperparameters
# E(theta)= alpha/(alpha+beta) = 0.7
# How sure we are: alpha+beta = ?
# To find the beta I choose the hyperparameters such that the 15th quantile of the
# Beta is approximately 0.5

alpha= 0.7*5.55
beta= 5.55-alpha
cat("alpha=",alpha,"beta=",beta)

qbeta(0.15, alpha, beta)
1 - pbeta(0.5, alpha, beta)


# Posterior parameters
alpha_n= alpha+M
beta_n= beta+n-M
cat("alpha_n=",alpha_n,"beta_n=",beta_n)


# Plot the prior
# The prior is the density of a Beta with parameters the hyperparameters
curve(dbeta(x, alpha, beta), from=0, to=1, col="green", lwd=2, 
      xlab="theta", ylab="density", ylim=c(0,5))

# Plot the likelihood
# The likelihood looks like the kernel of a Beta(M+1,n-M+1), therefore plotting 
#the density of a Beta with these parameters is equivalent to plot the normalized 
#version of the likelihood.
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=2, add=T)

# Plot the posterior
# The posterior is the density of a Beta with updated parameters
curve(dbeta(x, alpha_n, beta_n), from=0, to=1, col="blue", lwd=2, add=T)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)

# Note: the posterior is between the prior and the likelihood

# Posterior expected values: the mean of a Beta is: alpha/(alpha+beta)
# The posterior mean is between the prior and the sample mean
prior_mean=alpha/(alpha+beta)
post_mean= alpha_n/(alpha_n+beta_n)
sample_avg= mean(y)
cat("Prior mean:", prior_mean,"Posterior mean:",post_mean,"Sample average:",sample_avg)


# Posterior variance: the variance of a Beta is: (alpha*beta)/(alpha+beta+1)*(alpha+beta)^2
prior_var= (alpha*beta)/((alpha+beta+1)*(alpha+beta)^2)
post_var=(alpha_n*beta_n)/((alpha_n+beta_n+1)*(alpha_n+beta_n)^2)
cat("Prior variance:", prior_var,"Posterior variance:",post_var)


# Prior 95% credible interval
l0= qbeta(0.025, alpha, beta)
u0= qbeta(0.975, alpha, beta)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n= qbeta(0.025, alpha_n, beta_n)
u_n= qbeta(0.975, alpha_n, beta_n)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 15th quantiles
prior_15q=qbeta(0.15,alpha,beta)
post_15q=qbeta(0.15,alpha_n,beta_n)
cat("Prior 15th quantile:", prior_15q,", Posterior 15th quantile:",post_15q)



# Prediction

# The posterior predictive for a Bernoulli-Beta Model is: 
post_predictive=(alpha+M)/(alpha+beta+n)
cat("The posterior predictive is", post_predictive)
cat("The posterior predictive for the 20 observations of the new sample is", post_predictive*20)

# Example of the new sample
set.seed(123)
y_new= rbinom(20,1,post_mean)
cat("The new sample is", y_new)

cat("Students of the sample that sleep at least 8 hours:",sum(y_new))



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= pbeta(0.5,alpha_n,beta_n)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1=1-pbeta(0.5,alpha_n,beta_n)

# Pr(E0)
prior_prob0=pbeta(0.5,alpha,beta)

# Pr(E1)
prior_prob1=1-pbeta(0.5,alpha,beta)

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# strong


rm(list = ls())








############################### SECOND MODEL ###################################

# Data
y=c(rep(0,16), rep(1,11))
n=length(y)
M=sum(y)
cat("The sample size is",n,"and the students that sleep more than 8 hours are",M)

### Case 1 ###

# Choice of the prior
theta=seq(from=0.1,to=0.9,by=0.1)
cat("Theta:", theta)
prior=c(0.33,0.20,0.16,0.09,0.07,0.06,rep(0.03,3))
cat("Prior:", prior)
cat("The sum of the prior probabilities is", sum(prior))

# Cdf of the prior
cump=c()
cdf=function(p){
  for (j in 1:9){
    cump[j]=sum(p[1:j])
  }
  return(cump)
}

cumprior=cdf(prior)
cat("Cumulative distribution function of the prior:", cumprior)

# P(theta<=0.5)=0.85 and E(theta)=0.3
cat("P(theta<=0.5)=", cumprior[5])
prior_mean=weighted.mean(theta,prior)
cat("E(theta)=", prior_mean)

# Plot of the prior
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)

# Posterior
# posterior=likelihood*prior/marginal and is proportional to likelihood*prior
# The marginal is the integral (in this case sum) of likelihood*prior. 
# I applied this for each element
prop_post=c()
like=c()

for (j in 1:9){
  like[j]=(theta[j])^M*(1-theta[j])^(n-M)
  prop_post[j]=((theta[j])^M*(1-theta[j])^(n-M)*prior[j])
}

sum(prop_post) # not equal to 1 because I still have to divide for the marginal
post=prop_post/sum(prop_post)
cat("Posterior:", post)
cat("The sum of the posterior probabilities is", sum(post)) # equal to 1!

# Normalized version of the likelihood
cat("Likelihood:", like)
like2=like/sum(like) 
cat("Normalized version of the likelihood:", like2)
cat("The sum of the normalized likelihood probabilities is", sum(like2)) # equal to 1!



# Prior, posterior and likelihood plot
par(mfrow=c(2,2))
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)
plot(theta, post, main="Posterior", type="h", xlab="theta", ylab="probability", 
     col="blue", lwd=3)
plot(theta, like2, main="likelihood", type="h", xlab="theta", ylab="probability", 
     col="magenta", lwd=3)

par(mfrow=c(1,1))
ppl_table_graph=t(matrix(c(prior, post, like2), nrow=9, ncol=3))
barplot(ppl_table_graph, beside=T, xlab="theta", ylab="probability",
        col=c("green", "blue", "magenta"), ylim=c(0,0.45),
        names.arg=seq(from=0.1,to=0.9,by=0.1))

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Posterior mean
post_mean=weighted.mean(theta,post)
sample_avg=mean(y)
cat("Prior mean:", prior_mean, ", Posterior mean:", post_mean, 
    ", Sample average:", sample_avg)

# Posterior variance
prior_var=weighted.mean((theta-prior_mean)^2, prior)
post_var=weighted.mean((theta-post_mean)^2, post)
cat("Prior variance:", prior_var, ", Posterior variance:", post_var)


# Quantiles

# Posterior cdf
cumpost=cdf(post)
cat("Cumulative distribution function of the posterior:", cumpost)

# Discrete quantile function
my_quant=function(cfd, p){
  indx=which(cfd>=p)
  return(theta[indx[1]])
}

# Prior 95% credible interval
l0=my_quant(cumprior, 0.025)
u0=my_quant(cumprior, 0.975)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n=my_quant(cumpost, 0.025)
u_n=my_quant(cumpost, 0.975)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 85th quantile
prior_85q=my_quant(cumprior, 0.85)
post_85q=my_quant(cumpost, 0.85)
cat("Prior 85th quantile:", prior_85q,", Posterior 85th quantile:",post_85q)



# Prediction
# The predictive can be seen as posterior mean
# In particular the predictive is the integral (sum) of p(y_new=1, theta| y_1:n)
# It is possible to show that this is equal to the sum of p(y_new|theta)*p(theta|y_1:n)
# = (theta*posterior) = E(p(y_new|theta)|y_1:n) = post_mean

cat("The psterior predictive is:", post_mean)
cat("The posterior predictive for the 20 observations of the new sample is", post_mean*20)
# about 8 out of 20 in the new sample sleep at least 8 hours

# Example of the new sample
set.seed(123)
y_new=rbinom(20,1,prob=post_mean)
y_new

sum(y_new) # 8!



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= cumpost[5]
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= 1-post_prob0

# Pr(E0)
prior_prob0= cumprior[5]

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)





### Case 2 ###

# Choice of the prior
theta=seq(from=0.1,to=0.9,by=0.1)
cat("Theta:", theta)
prior=rep(1/9, 9)
cat("Prior:", prior)
cat("The sum of the prior probabilities is", sum(prior))

# Cdf of the prior
cumprior=cdf(prior)
cat("Cumulative distribution function of the prior:", cumprior)

# P(theta<=0.5) and E(theta)=0.5
cat("P(theta<=0.5)=", cumprior[5])
prior_mean=weighted.mean(theta,prior)
cat("E(theta)=", prior_mean)

# Plot of the prior
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)

# Posterior
# posterior=likelihood*prior/marginal and is proportional to likelihood*prior
# The marginal is the integral (in this case sum) of likelihood*prior. 
# I applied this for each element
prop_post=c()
like=c()

for (j in 1:9){
  like[j]=(theta[j])^M*(1-theta[j])^(n-M)
  prop_post[j]=((theta[j])^M*(1-theta[j])^(n-M)*prior[j])
}

sum(prop_post) # not equal to 1 because I still have to divide for the marginal
post=prop_post/sum(prop_post)
cat("Posterior:", post)
cat("The sum of the posterior probabilities is", sum(post)) # equal to 1!

# Normalized version of the likelihood
cat("Likelihood:", like)
like2=like/sum(like) 
cat("Normalized version of the likelihood:", like2)
cat("The sum of the normalized likelihood probabilities is", sum(like2)) # equal to 1!



# Prior, posterior and likelihood plot
par(mfrow=c(2,2))
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)
plot(theta, post, main="Posterior", type="h", xlab="theta", ylab="probability", 
     col="blue", lwd=3)
plot(theta, like2, main="likelihood", type="h", xlab="theta", ylab="probability", 
     col="magenta", lwd=3)

par(mfrow=c(1,1))
ppl_table_graph=t(matrix(c(prior, post, like2), nrow=9, ncol=3))
barplot(ppl_table_graph, beside=T, xlab="theta", ylab="probability",
        col=c("green", "blue", "magenta"), ylim=c(0,0.45),
        names.arg=seq(from=0.1,to=0.9,by=0.1))

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Posterior mean
post_mean=weighted.mean(theta,post)
sample_avg=mean(y)
cat("Prior mean:", prior_mean, ", Posterior mean:", post_mean, 
    ", Sample average:", sample_avg)

# Posterior variance
prior_var=weighted.mean((theta-prior_mean)^2, prior)
post_var=weighted.mean((theta-post_mean)^2, post)
cat("Prior variance:", prior_var, ", Posterior variance:", post_var)


# Quantiles

# Posterior cdf
cumpost=cdf(post)
cat("Cumulative distribution function of the posterior:", cumpost)

# Prior 95% credible interval
l0=my_quant(cumprior, 0.025)
u0=my_quant(cumprior, 0.975)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n=my_quant(cumpost, 0.025)
u_n=my_quant(cumpost, 0.975)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 85th quantile
prior_85q=my_quant(cumprior, 0.85)
post_85q=my_quant(cumpost, 0.85)
cat("Prior 85th quantile:", prior_85q,", Posterior 85th quantile:",post_85q)



# Prediction
# The predictive can be seen as posterior mean
# In particular the predictive is the integral (sum) of p(y_new=1, theta| y_1:n)
# It is possible to show that this is equal to the sum of p(y_new|theta)*p(theta|y_1:n)
# = (theta*posterior) = E(p(y_new|theta)|y_1:n) = post_mean

cat("The posterior predictive is:", post_mean)
cat("The posterior predictive for the 20 observations of the new sample is", post_mean*20)
# about 8 out of 20 in the new sample sleep at least 8 hours

# Example of the new sample
set.seed(123)
y_new=rbinom(20,1,prob=post_mean)
y_new

sum(y_new) # 8!



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= cumpost[5]
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= 1-post_prob0

# Pr(E0)
prior_prob0= cumprior[5]

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)




### Case 3 ###

# Choice of the prior
theta=seq(from=0.1,to=0.9,by=0.1)
cat("Theta:", theta)
prior=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.15, 0.25, 0.25, 0.20)
cat("Prior:", prior)
cat("The sum of the prior probabilities is", sum(prior))

# Cdf of the prior
cumprior=cdf(prior)
cat("Cumulative distribution function of the prior:", cumprior)

# P(theta<=0.5)=0.15 and E(theta)=0.7
cat("P(theta<=0.5)=", cumprior[5])
prior_mean=weighted.mean(theta,prior)
cat("E(theta)=", prior_mean)

# Plot of the prior
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)

# Posterior
# posterior=likelihood*prior/marginal and is proportional to likelihood*prior
# The marginal is the integral (in this case sum) of likelihood*prior. 
# I applied this for each element
prop_post=c()
like=c()

for (j in 1:9){
  like[j]=(theta[j])^M*(1-theta[j])^(n-M)
  prop_post[j]=((theta[j])^M*(1-theta[j])^(n-M)*prior[j])
}

sum(prop_post) # not equal to 1 because I still have to divide for the marginal
post=prop_post/sum(prop_post)
cat("Posterior:", post)
cat("The sum of the posterior probabilities is", sum(post)) # equal to 1!

# Normalized version of the likelihood
cat("Likelihood:", like)
like2=like/sum(like) 
cat("Normalized version of the likelihood:", like2)
cat("The sum of the normalized likelihood probabilities is", sum(like2)) # equal to 1!



# Prior, posterior and likelihood plot
par(mfrow=c(2,2))
plot(theta, prior, main="Prior", type="h", xlab="theta", ylab="probability", 
     col="green", lwd=3)
plot(theta, post, main="Posterior", type="h", xlab="theta", ylab="probability", 
     col="blue", lwd=3)
plot(theta, like2, main="likelihood", type="h", xlab="theta", ylab="probability", 
     col="magenta", lwd=3)

par(mfrow=c(1,1))
ppl_table_graph=t(matrix(c(prior, post, like2), nrow=9, ncol=3))
barplot(ppl_table_graph, beside=T, xlab="theta", ylab="probability",
        col=c("green", "blue", "magenta"), ylim=c(0,0.45),
        names.arg=seq(from=0.1,to=0.9,by=0.1))

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Posterior mean
post_mean=weighted.mean(theta,post)
sample_avg=mean(y)
cat("Prior mean:", prior_mean, ", Posterior mean:", post_mean, 
    ", Sample average:", sample_avg)

# Posterior variance
prior_var=weighted.mean((theta-prior_mean)^2, prior)
post_var=weighted.mean((theta-post_mean)^2, post)
cat("Prior variance:", prior_var, ", Posterior variance:", post_var)


# Quantiles

# Posterior cdf
cumpost=cdf(post)
cat("Cumulative distribution function of the posterior:", cumpost)

# Prior 95% credible interval
l0=my_quant(cumprior, 0.025)
u0=my_quant(cumprior, 0.975)
cat("The 95% credible interval of the prior is (",l0,",",u0,")")

# Posterior 95% credible interval
l_n=my_quant(cumpost, 0.025)
u_n=my_quant(cumpost, 0.975)
cat("The 95% credible interval of the posterior is (",l_n,",",u_n,")")


# 15th quantile
prior_15q=my_quant(cumprior, 0.15)
post_15q=my_quant(cumpost, 0.15)
cat("Prior 15th quantile:", prior_15q,", Posterior 15th quantile:",post_15q)



# Prediction
# The predictive can be seen as posterior mean
# In particular the predictive is the integral (sum) of p(y_new=1, theta| y_1:n)
# It is possible to show that this is equal to the sum of p(y_new|theta)*p(theta|y_1:n)
# = (theta*posterior) = E(p(y_new|theta)|y_1:n) = post_mean

cat("The psterior predictive is:", post_mean)
cat("The posterior predictive for the 20 observations of the new sample is", post_mean*20)
# about 9 out of 20 in the new sample sleep at least 8 hours

# Example of the new sample
set.seed(123)
y_new=rbinom(20,1,prob=post_mean)
y_new

sum(y_new) # 10



# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= cumpost[5]
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= 1-post_prob0

# Pr(E0)
prior_prob0= cumprior[5]

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)


rm(list = ls())







############################### MODEL 3 ########################################

### PRIOR ANALYSIS ###

# To fill the tables all at once I set a for loop
# Set the vectors of hyperparameters
c=c(-1,0,1)
s=c(4,1,0.1)

# Set up the two tables
m_theta=matrix(nrow=3,ncol=3)
p0.5_theta=matrix(nrow=3, ncol=3)

# For loop to draw the iid samples and fill the tables
for (i in 1:length(c)){
  for (j in 1:length(s)){
    set.seed(123)
    # sample X iid from the N(c,s^2)
    x=rnorm(10000,mean=c[i],sd=s[j])
    # transform the X sample into the relative theta
    # Since the X are iid, also the theta are iid
    theta=1/(1+exp(-x))
    
    m_theta[j,i]=mean(theta)
    p0.5_theta[j,i]=mean(theta<0.5)
  }
}


# E(theta) with different hyperparameters
m_theta

#P(theta<0.5) with different hyperparameters 
p0.5_theta


# to verify it:
c=c(-1,0,1)
s=c(4,1,0.1)

m_theta1=matrix(nrow=3,ncol=3)
p0.5_theta1=matrix(nrow=3, ncol=3)

for (i in 1:length(c)){
  for (j in 1:length(s)){
    set.seed(123)
    th=runif(10000)
    p3=1/(s[j]*sqrt(2*3.14))*exp((-1/(2*s[j]^2))*(log(th/(1-th))-c[i])^2)*1/(th*(1-th))
    theta=(sample(th,10000,replace=T,prob=p3))
    m_theta1[j,i]= mean(theta)
    p0.5_theta1[j,i]=mean(theta<0.5)
  }
}


m_theta1
p0.5_theta1


rm(list = ls())




### CASE 1 ###

# Fix the hyperparameters
c=-1
s=1

# Prior function
model_prior=function(th, n, M, c, s){
  if (th<=0|th>=1) {
    return(-Inf)
  }
  out=1/(s*sqrt(2*3.14))*exp((-1/(2*s^2))*(log(th/(1-th))-c)^2)*1/(th*(1-th))
  return(out)
}

# Data and sufficient statistics
y=c(rep(0,16), rep(1,11))
n=length(y)
M=sum(y)
cat("The sample size is",n,"and the students that sleep more than 8 hours are",M)


# Posterior= target of the MH
model_posterior=function(th, n, M, c, s){
  if (th<=0|th>=1) {
    return(-Inf)
  }
  out= (th)^M*(1-th)^(n-M) * 1/(s*sqrt(2*3.14))*exp((-1/(2*s^2))*(log(th/(1-th))-c)^2)*1/(th*(1-th))
  return(out)
}

# Plot
gr_th=seq(0,1,0.001)
mp=c()

for (i in 1:length(gr_th)){
  mp[i]=model_posterior(gr_th[i], n, M, c, s)
}

plot(gr_th, mp, lwd=3, type="l", col="blue", main="Posterior density",
     xlab="theta", ylab="density")


# MH function
MH_post3=function(G, burnin, thin, n, M, c, s, eta,th0=0.5){
  
  #Set the number of iterations
  iterations= burnin+thin*G
  g= 1
  
  # Define the output vector
  theta= vector("numeric", G)
  current_state= th0 #the current state of the chain is th_0
  
  acc=0; #count how many time I accept a transition
  
  for (iter in 1:iterations) {
    prop_state = rnorm(1,mean=current_state,sd=eta) #My proposal are drawn from a Normal
    
    if(prop_state<=0|prop_state>=1){# Values I cannot accept
      alpha= -Inf
    }else{
      alpha= min(1, (model_posterior(prop_state, n, M, c, s))/
                   (model_posterior(current_state, n, M, c, s)))
    }
    
    u= runif(1)
    if (u < alpha){
      #Accept the move
      current_state= prop_state
      acc= acc+1
    } # the else is not needed because nothing changes
 
    if( (iter>burnin) & (iter%%thin==0) ){
      # if iter is larger than burn
      # and iter is multiple of thin
      # Save the current_state
      theta[g]= current_state
      g= g+1
    }
  }
  cat("I accepted the ", acc/iterations*100,"% of the proposed transition\n")
  return(theta)
}

# Optimal variance sample
set.seed(123)
th_sample1= MH_post3(G=5000, burnin = 1000, thin = 10, n, M, c, s, eta=0.44, th0=0.5)

# Wrong variance samples
set.seed(123)
wrong_th_sample1= MH_post3(G=5000, burnin = 1000, thin = 10, n, M, c, s, eta=4, th0=0.5)

set.seed(123)
wrong_th_sample2= MH_post3(G=5000, burnin = 1000, thin = 10, n, M, c, s, eta=0.01, th0=0.5)




# Convergence and autocorrelation check for sample 1
# trace plot
plot(th_sample1,type="l")
# auto correlation function
acf(th_sample1)

# Cumulative mean
cmst <- cumsum(th_sample1)
G <- 5000
plot(1:G,cmst/1:G,type="b",main="cumulative mean",pch="*")


# Convergence and autocorrelation check for wrong sample 1
# trace plot
plot(wrong_th_sample1,type="l")
# auto correlation function
acf(wrong_th_sample1)

# Cumulative mean
cmst <- cumsum(wrong_th_sample1)
G <- 5000
plot(1:G,cmst/1:G,type="b",main="cumulative mean",pch="*")


# Convergence and autocorrelation check for wrong sample 2
# trace plot
plot(wrong_th_sample2,type="l")
# auto correlation function
acf(wrong_th_sample2)

# Cumulative mean
cmst <- cumsum(wrong_th_sample2)
G <- 5000
plot(1:G,cmst/1:G,type="b",main="cumulative mean",pch="*")



# Summary and plots with coda
library(coda)
th.post.mc= mcmc(th_sample1)
summary(th.post.mc)
plot(th.post.mc)
acfplot(th.post.mc)
cumuplot(th.post.mc)

# Geweke diagnostic
geweke.diag(th.post.mc)
2*min(pnorm(0.3524), 1-pnorm(0.3524)) #the p-value is high
geweke.plot(th.post.mc, frac1 = 0.1, frac2 = 0.5, nbins = 20 )

# Gelman and Rubin
# Sample new chains and plot them
set.seed(123)
th.post2= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post3= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post4= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post5= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post6= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post7= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post8= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post9= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post10= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

ten.mh <-mcmc.list(th.post.mc,th.post2,th.post3,th.post4,th.post5,th.post6,
                   th.post7,th.post8,th.post9,th.post10)

# Plots of the chains
plot(ten.mh)
acfplot(ten.mh,type="h")

# Gelman and Rubin diagnostic
gelman.diag(ten.mh) # The potential reduction factor is 1, the chains are stationary

gelman.plot(ten.mh) # how the potential reduction factor changes with the iterations

# Effective sample size
effectiveSize(th.post.mc) # On the single chain (the first)
effectiveSize(ten.mh) # On the ten chains


# Bayesian analysis

# Histogram
hist(th_sample1,main="Posterior density of theta", freq=F, col="blue")


# Prior plot
gr_th=seq(0,1,0.001)
mpr=c()

for (i in 1:length(gr_th)){
  mpr[i]=model_prior(gr_th[i], n, M, c, s)
}

plot(gr_th, mpr, lwd=3, type="l", col="green", ylim=c(0,5), xlab="theta", ylab="density")

# likelihood plot
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=3, add=T)

# Posterior: kernel density plot
lines(density(th_sample1),col="blue",lwd=3)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)



# Posterior mean
# I took the prior mean from the table at the beginning
post_mean=mean(th_sample1)
sample_avg= mean(y)
cat("Prior mean: 0.3027046, Posterior mean:",post_mean,"Sample average:",sample_avg)

# Posterior variance
post_var=var(th_sample1)
cat("Posterior variance:",post_var)

# Posterior 95% credible intervals
i_n=quantile(th_sample1, prob=c(0.025,0.975))
cat("The 95% credible interval of the posterior is:", i_n)

# Posterior 85th quantile
post_85q=quantile(th_sample1, prob=0.85)
cat("Posterior 85th quantile:",post_85q)




# Prediction
G=20
y_star <- vector(length=G)

set.seed(123)
for(g in 1:G){
  y_star[g] <- rbinom(1,1, prob=th_sample1[g])
}

cat("Students of the sample that sleep at least 8 hours:",sum(y_star))




# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= mean(th_sample1<=0.5)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= mean(th_sample1>0.5)

# Pr(E0)
prior_prob0=0.8424 #from the prior table 2

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# "not worth more than bare mentioning"




### CASE 2 ###

# Fix the hyperparameters
c=0
s=4


# Posterior plot
gr_th=seq(0,1,0.001)
mp=c()

for (i in 1:length(gr_th)){
  mp[i]=model_posterior(gr_th[i], n, M, c, s)
}

plot(gr_th, mp, lwd=3, type="l", col="blue", main="Posterior density",
     xlab="theta", ylab="density")


# MH sample using the function: optimal variance sample
set.seed(123)
th_sample1= MH_post3(G=5000, burnin = 1000, thin = 10, n, M, c, s, eta=0.47, th0=0.5)



# Convergence and autocorrelation check for sample 1
# trace plot
plot(th_sample1,type="l")
# auto correlation function
acf(th_sample1)

# Cumulative mean
cmst <- cumsum(th_sample1)
G <- 5000
plot(1:G,cmst/1:G,type="b",main="cumulative mean",pch="*")


# Coda
library(coda)
th.post.mc= mcmc(th_sample1)

# Geweke diagnostic
geweke.diag(th.post.mc)
# P-value
2*min(pnorm(0.798), 1-pnorm(0.798)) 
# Geweke plot
geweke.plot(th.post.mc, frac1 = 0.1, frac2 = 0.5, nbins = 20)

# Gelman and Rubin
# Sample new chains and plot them
set.seed(123)
th.post2= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post3= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post4= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post5= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post6= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post7= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post8= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post9= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post10= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                         n, M, c, s, eta=0.44, th0= runif(1,0,1)))

ten.mh <-mcmc.list(th.post.mc,th.post2,th.post3,th.post4,th.post5,th.post6,
                   th.post7,th.post8,th.post9,th.post10)

# Plots of the chains
plot(ten.mh)
acfplot(ten.mh,type="h")

# Gelman and Rubin diagnostic
gelman.diag(ten.mh) # The potential reduction factor is 1, the chains are stationary
gelman.plot(ten.mh) # how the potential reduction factor changes with the iterations

# Effective sample size
effectiveSize(th.post.mc) # On the first chain
effectiveSize(ten.mh) # On the ten chains


# Bayesian analysis

# Histogram
hist(th_sample1,main="Posterior density of theta", freq=F, col="blue")


# Prior plot
gr_th=seq(0,1,0.001)
mpr=c()

for (i in 1:length(gr_th)){
  mpr[i]=model_prior(gr_th[i], n, M, c, s)
}

plot(gr_th, mpr, lwd=3, type="l", col="green", ylim=c(0,5), xlab="theta", ylab="density")

# likelihood plot
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=3, add=T)

# Posterior: kernel density plot
lines(density(th_sample1),col="blue",lwd=3)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Posterior mean
# I took the prior mean from the table at the beginning
post_mean=mean(th_sample1)
sample_avg= mean(y)
cat("Prior mean: 0.4990392, Posterior mean:",post_mean,"Sample average:",sample_avg)

# Posterior variance
post_var=var(th_sample1)
cat("Posterior variance:",post_var)

# Posterior 95% credible intervals
i_n=quantile(th_sample1, prob=c(0.025,0.975))
cat("The 95% credible interval of the posterior is:", i_n)

# Posterior 85th quantile
post_85q=quantile(th_sample1, prob=0.85)
cat("Posterior 85th quantile:",post_85q)


# Prediction
G=20
y_star <- vector(length=G)

set.seed(123)
for(g in 1:G){
  y_star[g] <- rbinom(1,1, prob=th_sample1[g])
}

cat("Students of the sample that sleep at least 8 hours:",sum(y_star))




# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= mean(th_sample1<=0.5)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= mean(th_sample1>0.5)

# Pr(E0)
prior_prob0=0.5041 #from the prior table 2

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# "substantial"




### CASE 3 ###

# Fix the hyperparameters
c=1
s=1


# Posterior plot
gr_th=seq(0,1,0.001)
mp=c()

for (i in 1:length(gr_th)){
  mp[i]=model_posterior(gr_th[i], n, M, c, s)
}

plot(gr_th, mp, lwd=3, type="l", col="blue", main="Posterior density",
     xlab="theta", ylab="density")


# MH sample using the function: optimal variance sample
set.seed(123)
th_sample1= MH_post3(G=5000, burnin = 1000, thin = 10, n, M, c, s, eta=0.45, th0=0.5)


# Convergence and autocorrelation check for sample 1
# trace plot
plot(th_sample1,type="l")
# auto correlation function
acf(th_sample1)

# Cumulative mean
cmst <- cumsum(th_sample1)
G <- 5000
plot(1:G,cmst/1:G,type="b",main="cumulative mean",pch="*")


# Coda
library(coda)
th.post.mc= mcmc(th_sample1)

# Geweke diagnostic
geweke.diag(th.post.mc)
# P-value
2*min(pnorm(0.4806), 1-pnorm(0.4806)) 
# Geweke plot
geweke.plot(th.post.mc, frac1 = 0.1, frac2 = 0.5, nbins = 20)

# Gelman and Rubin
# Sample new chains and plot them
set.seed(123)
th.post2= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post3= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post4= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post5= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post6= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post7= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post8= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post9= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                        n, M, c, s, eta=0.44, th0= runif(1,0,1)))

th.post10= mcmc(MH_post3(G=5000, burnin = 1000, thin = 10, 
                         n, M, c, s, eta=0.44, th0= runif(1,0,1)))

ten.mh <-mcmc.list(th.post.mc,th.post2,th.post3,th.post4,th.post5,th.post6,
                   th.post7,th.post8,th.post9,th.post10)

# Plots of the chains
plot(ten.mh)
acfplot(ten.mh,type="h")

# Gelman and Rubin diagnostic
gelman.diag(ten.mh) # The potential reduction factor is 1, the chains are stationary
gelman.plot(ten.mh) # how the potential reduction factor changes with the iterations

# Effective sample size
effectiveSize(th.post.mc) # On the first chain
effectiveSize(ten.mh) # On the ten chains


# Bayesian analysis

# Histogram
hist(th_sample1,main="Posterior density of theta", freq=F, col="blue")


# Prior plot
gr_th=seq(0,1,0.001)
mpr=c()

for (i in 1:length(gr_th)){
  mpr[i]=model_prior(gr_th[i], n, M, c, s)
}

plot(gr_th, mpr, lwd=3, type="l", col="green", ylim=c(0,5), xlab="theta", ylab="density")

# likelihood plot
curve(dbeta(x, M+1, n-M+1), from=0, to=1, col="magenta", lwd=3, add=T)

# Posterior: kernel density plot
lines(density(th_sample1),col="blue",lwd=3)

legend("topright",c("prior","likelihood","posterior"), 
       lty=c(1,1,1),
       col=c("green","magenta","blue"),
       cex=0.8)


# Posterior mean
# I took the prior mean from the table at the beginning
post_mean=mean(th_sample1)
sample_avg= mean(y)
cat("Prior mean: 0.6963821, Posterior mean:",post_mean,"Sample average:",sample_avg)

# Posterior variance
post_var=var(th_sample1)
cat("Posterior variance:",post_var)

# Posterior 95% credible intervals
i_n=quantile(th_sample1, prob=c(0.025,0.975))
cat("The 95% credible interval of the posterior is:", i_n)

# Posterior 16th quantile
post_16q=quantile(th_sample1, prob=0.16)
cat("Posterior 16th quantile:",post_16q)


# Prediction
G=20
y_star <- vector(length=G)

set.seed(123)
for(g in 1:G){
  y_star[g] <- rbinom(1,1, prob=th_sample1[g])
}

cat("Students of the sample that sleep at least 8 hours:",sum(y_star))




# Hypothesis testing

# Pr(E0|y_1:n)
post_prob0= mean(th_sample1<=0.5)
cat("The posterior probability of E0 is", post_prob0)

# Pr(E1|y_1:n)
post_prob1= mean(th_sample1>0.5)

# Pr(E0)
prior_prob0=0.1612 #from the prior table 2

# Pr(E1)
prior_prob1= 1-prior_prob0

# Bayes factor
BF= (post_prob0/post_prob1)/(prior_prob0/prior_prob1)
cat("The Bayes Factor is",BF)
# "strong
