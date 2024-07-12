# data: 1, 1, 2, 0, 1, 2, 0, 5, 0, 1
# Likelihood
# y_i ~ Poisson(lambda)
# Prior
# lambda ~ Gamma(10,10)
# Posterior
# lambda|y ~ Gamma(23,20)
library(truncnorm)
library(ggplot2)

theta = seq(0,1,length = 1000)
dat = data.frame(theta = theta)
dat$dist_an = rbeta(1000,23,20)
dat$dist_beta = rbeta(1000,5,5)

library(reshape2)

ggplot(melt(dat,id = "theta"),aes(x = value,group = variable,colour = variable))+
  geom_density()+
  theme_bw()+
  facet_wrap(~variable)


y <- c(1, 1, 2, 0, 1, 2, 0, 5, 0, 1)
y
# y_i ~ Poisson(lambda)
# lambda ~ Gamma(10,10)
nsamp <- 20000
mcmc <- function(y,step_size){
  nsamp <- 20000
  lambda_chain <- rep(NA,nsamp)
  reject <- 0
  
  # Intialization
  lambda_chain[1] <- rgamma(1,10,10)
  i <- 1
  
  while(i < nsamp){
    proposal_lambda <- 
      rtruncnorm(1,a=0,b=Inf,mean=lambda_chain[i],sd=step_size)
    lkl_proposal <- prod(dpois(y,proposal_lambda))
    prior_proposal <- dgamma(proposal_lambda,10,10)
    forward_density <- 
      dtruncnorm(proposal_lambda,a=0,b=Inf,mean=lambda_chain[i],sd=step_size)
    lkl_current <- prod(dpois(y,lambda_chain[i]))
    prior_current <- dgamma(lambda_chain[i],10,10)
    backward_density <- 
      dtruncnorm(lambda_chain[i],a=0,b=Inf,mean=proposal_lambda,sd=step_size)
    H <- (lkl_proposal*prior_proposal*backward_density)/(lkl_current*prior_current*forward_density)
    astr <- runif(1,0,1)
    if(astr<min(1,H)){
      lambda_chain[i+1] <- proposal_lambda
      i <- i+1
    }else{
      reject <- reject+1
    }
  }
  return(list(lambda_chain=lambda_chain,reject=reject))
}

chain1 <- mcmc(y=y,step_size = 0.2)
chain2 <- mcmc(y=y,step_size = 0.2)
chain3 <- mcmc(y=y,step_size = 0.2)
chain4 <- mcmc(y=y,step_size = 0.2)

rejection_count <- chain1$reject
rate <- (rejection_count/(nsamp+rejection_count))*100
rate

chains <- data.frame(matrix(nrow = nsamp,ncol=4))
colnames(chains) <- c("chain 1","chain 2","chain 3","chain 4")
chains$`chain 1`<- chain1$lambda_chain
chains$`chain 2`<- chain2$lambda_chain
chains$`chain 3` <- chain3$lambda_chain
chains$`chain 4` <- chain4$lambda_chain

chains$sample <- 1:nsamp
library(ggplot2)
library(reshape2)
ggplot(melt(chains,id="sample"),aes(x=sample,y=value,group=variable,color=variable))+
  geom_line()

chain.m <- melt(chains,id="sample")
chain.m <- rbind(chain.m,data.frame(sample=1:nsamp*4,
                                    variable=rep("0",nsamp*4),
                            value=rgamma(nsamp*4,23,20)))
chain.m$type <- factor(c(rep("MCMC",nsamp*4),rep("Analytical",nsamp*4)))

ggplot(chain.m,aes(x=value,group=type,color=type))+
  geom_density(size = 1.2)

#####
## Importance sampling ###
y

nsamp <- 200000
proposal_lambda <- rgamma(nsamp,5,5)
weights <- rep(NA,nsamp)
for(i in 1:nsamp){
  likelihood <- prod(dpois(y,lambda=proposal_lambda[i]))
  prior <- dgamma(proposal_lambda[i],10,10)
  proposal_dens <- dgamma(proposal_lambda[i],5,5)
  weights[i] <- likelihood*prior/proposal_dens
}
  
posterior_lambda <- sample(proposal_lambda,size=5000,
                           prob=weights)
df.post <- data.frame(post=posterior_lambda)

analytic_post <- rgamma(5000,10+sum(y),10+length(y))
df.post$analytical <- analytic_post
library(ggplot2)
ggplot(df.post,aes(x=post))+
  geom_density(adjust=1.5)+
  geom_density(aes(x=analytical),color="red",adjust=1.5)

##############################


y <- rpois(1000,lambda = 1.3)

nsamp <- 2000
mcmc <- function(y,step_size){
  nsamp <- 2000
  lambda_chain <- rep(NA,nsamp)
  reject <- 0
  
  # Intialization
  lambda_chain[1] <- rgamma(1,10,10)
  i <- 1
  
  while(i < nsamp){
    proposal_lambda <- 
      rtruncnorm(1,a=0,b=Inf,mean=lambda_chain[i],sd=step_size)
    
    log_lkl_proposal <- sum(dpois(y,proposal_lambda,log = TRUE))
    log_prior_proposal <- dgamma(proposal_lambda,10,10,log = TRUE)
    log_forward_density <- 
      log(dtruncnorm(proposal_lambda,a=0,b=Inf,mean=lambda_chain[i],sd=step_size))
    
    log_lkl_current <- sum(dpois(y,lambda_chain[i],log = TRUE))
    log_prior_current <- dgamma(lambda_chain[i],10,10,log=TRUE)
    log_backward_density <- 
      log(dtruncnorm(lambda_chain[i],a=0,b=Inf,mean=proposal_lambda,sd=step_size))
    
    H <- exp((log_lkl_proposal+log_prior_proposal+log_backward_density)-(log_lkl_current+log_prior_current+log_forward_density))
    astr <- runif(1,0,1)
    if(astr<min(1,H)){
      lambda_chain[i+1] <- proposal_lambda
      i <- i+1
    }else{
      reject <- reject+1
    }
  }
  return(list(lambda_chain=lambda_chain,reject=reject))
}

chain1 <- mcmc(y=y,step_size = 0.008)
chain2 <- mcmc(y=y,step_size = 0.008)
chain3 <- mcmc(y=y,step_size = 0.008)
chain4 <- mcmc(y=y,step_size = 0.008)

rejection_count <- chain1$reject
rate <- (rejection_count/(nsamp+rejection_count))*100
rate

chains <- data.frame(matrix(nrow = 2000,ncol=4))
colnames(chains) <- c("chain 1","chain 2","chain 3","chain 4")
chains$`chain 1`<- chain1$lambda_chain
chains$`chain 2`<- chain2$lambda_chain
chains$`chain 3` <- chain3$lambda_chain
chains$`chain 4` <- chain4$lambda_chain

chains$sample <- 1:nsamp
library(ggplot2)
library(reshape2)
ggplot(melt(chains,id="sample"),aes(x=sample,y=value,group=variable,color=variable))+
  geom_line()+scale_x_continuous(limits = c(1,2000))

quantile(melt(chains,id="sample")$value,probs=c(.025,0.975))




chain.m <- melt(chains,id="sample")
chain.m <- rbind(chain.m,data.frame(sample=1:8000,
                                    variable=rep("0",8000),
                                    value=rgamma(8000,23,20)))
View(chain.m)
chain.m$type <- factor(c(rep("MCMC",8000),rep("Analytical",8000)))
View(chain.m)
ggplot(chain.m,aes(x=value,group=type,color=type))+
  geom_density()

###############################



