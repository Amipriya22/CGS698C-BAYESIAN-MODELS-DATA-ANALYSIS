library(ggplot2)
y <- rnorm(50,300,10)
hist(y)

sigma <- 10
mu <- seq(from=200,to=400,by=0.05)
likelihoods <- data.frame(mu=mu)
likelihoods$lkl <- NA
for(i in 1:length(mu)){
  likelihoods$lkl[i] <- prod(dnorm(y,mu[i],sd=10))
}
ggplot(likelihoods,aes(x=mu,y=lkl))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("Likelihood")

ggplot(likelihoods,aes(x=mu,y=lkl))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("Likelihood")+
  scale_x_continuous(limits = c(275,325))

ggplot(likelihoods,aes(x=mu,y=lkl))+geom_line(size=1,color="blue")+
  theme_bw()+xlab(expression(mu))+ylab("Likelihood")+
  scale_x_continuous(limits = c(275,325))+
  geom_vline(xintercept = mean(y),color="red",linetype="dashed")

# The priors

likelihoods$prior_density <- NA
for(i in 1:length(mu))
{
  likelihoods$prior_density[i] = dnorm(likelihoods$mu[i],mean = 350,sd = 10)
}
ggplot(likelihoods,aes(x=mu,y=prior_density))+geom_line(size=1,color="red")+
  theme_bw()+xlab(expression(mu))+ylab("Prior density")

View(likelihoods)

library(reshape2)
df.lkl_prior <- melt(likelihoods,id=c("mu"))

View(df.lkl_prior)

df.lkl_prior$variable <- ifelse(df.lkl_prior$variable=="lkl","Likelihood",
                                "Prior density")
View(df.lkl_prior)
ggplot(df.lkl_prior,aes(x=mu,y=value,color=variable))+geom_line(size=1)+
  theme_bw()+xlab(expression(mu))+ylab("")+
  scale_x_continuous(limits = c(250,400))+
  facet_wrap(~variable,scales = "free_y",ncol = 1)+
  scale_color_manual(values = c("blue","orange"))

# Sample from the priors
mu <- rnorm(2000,300,50)
sigma <- rep(10,2000)

# Create a dataframe to store the simulated data
# One way is to simply generate a datapoint
# corresponding to each value of mu
xsim <- rep(NA,length(mu))
for(i in 1:length(mu)){
  xsim[i]<- rnorm(1,mu[i],sd=sigma[i])
}
hist(xsim)

# But we want to make the simulated data
# as comparable as we can to the observed data
# Our observed data y contained 50 observations
# So, we should simulate samples containing 50 observations
# for each value of mu
# Thus, we need to generate 2000 samples
# each containing N observations (datapoints)
N <- 50 
# I will keep it same as the number of observations in our data y
df.sim <- data.frame(sample = rep(1:2000,each=N),
                     mu=rep(mu,each=N),sigma=rep(sigma,each=N),
                     observation = rep(1:N,2000))
View(df.sim)
df.sim$ysim <- NA
for(i in 1:length(mu)){
  df.sim[df.sim$sample==i,]$ysim <- rnorm(N,mean=mu[i],sd=sigma[i])
}
hist(df.sim$ysim)

ggplot(subset(df.sim,sample<10),aes(x=ysim))+geom_histogram()+facet_wrap(~sample)

#Prior predictive checks

ggplot(subset(df.sim,sample<10),aes(x=ysim))+geom_histogram()+
  facet_wrap(~sample)+
  geom_vline(xintercept = mean(y),color="red",linetype="dashed",
             size=1)

# creating summary data frame and visualising it using histograms
library(plyr)
library(dplyr)

# grouped by sample store average value of ysim 
#for eaach sample or each mu generated above .
df.sim.summary <- df.sim %>% group_by(sample) %>%
  summarise(meanRT=mean(ysim),sdRT=sd(ysim))

View(df.sim.summary)

ggplot(df.sim.summary,aes(x=meanRT))+
  geom_histogram(fill="white",color="black")+
  theme_bw()+xlab("Sample means of the simulated data")

#putting vertical line of mean of actual or observed data
ggplot(df.sim.summary,aes(x=meanRT))+
  geom_histogram(fill="white",color="black")+
  theme_bw()+xlab("Sample means of the simulated data")+
  geom_vline(xintercept=mean(y),color="red",
             linetype="dashed",size=1)


## Prior predictions of the model

# You may need either complete simulated data for prior predictions
# Or just a summary statistic of the simulated samples
# Prior predictions of our model

ggplot(df.sim.summary,aes(x=meanRT))+
  geom_histogram(fill="white",color="black")+
  theme_bw()+
  xlab("Average reading time predicted by the model \n (in milliseconds)")

###  Parameter estimation

## unnormalised posterior estimation
# Given the likelihood and the prior density functions, we should be able to estimate the unnormalized
#posterior distribution using Bayes’ rule:
#  Posterior ∝ Likelihood ×Prior
# Unnormalized posterior distribution of µ:
#  p′(µ|y) = L(µ|y)p(µ) i.e no division by sum(L(µ|y)p(µ)).

#adding another coloumn of unnormalised posterior distribution in likelihood dataframe

likelihoods$posterior_unnorm <- likelihoods$lkl*likelihoods$prior_density
ggplot(likelihoods,aes(x=mu,y=posterior_unnorm))+geom_line(size=1,color="black")+
  theme_bw()+xlab(expression(mu))+ylab("Posterior density \n Unnormalized")+
  scale_x_continuous(limits = c(250,350))

#again as before if we melt this data frame to df.lkl_prior we will now have
# 3 variables (lkl---> likelihood, prior,posterior) initialy 2variables
# each having value of their as their probability

df.lkl_prior<-melt(likelihoods,id=c("mu"))
df.lkl_prior$variable<-
  ifelse(df.lkl_prior$variable=="lkl","Likelihood",
         ifelse(df.lkl_prior$variable=="prior_density",
                "Prior density","Unnormalized posterior density"))

#plotting their distributions grouped by the variable 

ggplot(df.lkl_prior,aes(x=mu,y=value,color=variable))+geom_line(size=1)+
  theme_bw()+xlab(expression(mu))+ylab("")+
  scale_x_continuous(limits=c(250,400))+
  facet_wrap(~variable,scales="free_y",ncol=1)+
  scale_color_manual(values=c("blue","orange","black"))+
  theme(legend.position="none")

#Analytically-derived posterior distribution

# Parameters of the posterior distribution

mu_0 <- 350
sigma_0 <- 50
sigma <- 10
n <- length(y)
sigma_post <- 1/sqrt((1/sigma_0^2)+(n/sigma^2))
mu_post <- (sigma_post^2)*((mu_0/sigma_0^2)+(sum(y)/sigma^2))
post_samples <- rnorm(10000,mu_post,sigma_post) #this is randomvariable of post dist.
hist(post_samples)

#Does this look similar to the unnormalized posterior density graph?

df.post_samples <- data.frame(post_samples)
View(df.post_samples)

ggplot(df.post_samples,aes(x=post_samples))+
  geom_density(size=1)+theme_bw()+
  scale_x_continuous(limits = c(275,325))

ggplot(likelihoods,aes(x=mu,y=posterior_unnorm))+geom_line(size=1,color="black")+
  theme_bw()+xlab(expression(mu))+ylab("Posterior density \n Unnormalized")+
  scale_x_continuous(limits = c(275,325))

## 8 Posterior predictions of the model

# 1. Draw a lot of samples from the posterior distribution.
# 2. Simulate data (50 observations) from the model for each sample.

mu_samples <- rnorm(2000,mu_post,sigma_post)
sigma <- rep(10,2000)
N <- 50 # I will keep it same as the number of observations in our data y
df.pred <- data.frame(sample = rep(1:2000,each=N),
                      mu=rep(mu_samples,each=N),sigma=rep(sigma,each=N),
                      observation = rep(1:N,2000))
df.pred$ypred <- NA
for(i in 1:length(mu_samples)){
  df.pred[df.pred$sample==i,]$ypred <- rnorm(N,mean=mu_samples[i],sd=sigma[i])
}
ggplot(df.pred,aes(x=ypred,group=sample))+
  geom_density(alpha=0.0001)+theme_bw()

View(df.pred)


obs <- subset(df.pred,sample==1)
obs$ypred <- y

View(obs)
ggplot(df.pred,aes(x=ypred,group=sample))+
  geom_density(alpha=0.0001,color="gray")+
  geom_density(data=obs,aes(x=ypred),color="red",size=1)

ggplot(subset(df.pred,sample<200),aes(x=ypred,group=sample))+
  geom_density(alpha=0.0001,color="gray")+
  geom_density(data=obs,aes(x=ypred),color="red",size=1)

##  9 Bayesian work flow using brms

# 9.1 Model

# likelihood -----> y ~N(mu,sigma) ,
# priors ------> mu ~ N(350,50),
# sigma = 10

# 9.2 Check prior predictions (again check for it)

# 9.3 Prepapre data

dat <- data.frame(trial=1:length(y),y=y)
head(dat)

#  9.4 Specify the model in the ‘brm’ function

library(brms)

m1 <- brm(y ~ 1, data = dat,
          family = gaussian(),
          prior = c(prior(normal(350,50),class=Intercept),
                    prior(constant(10),class=sigma))
)

summary(m1)

##  9.5 Visualize the posterior samples

plot(m1,pars = c("b_Intercept"))

##  9.6 Extract the posterior samples
post.samples <- posterior_samples(m1)

View(post.samples)

hist(post.samples$b_Intercept) #our model will show peak at that mu where the predicted data (y_pred) is close to y (observed data)

##  9.7 Check posterior predictions
pp_check(m1,ndraws = 100)
