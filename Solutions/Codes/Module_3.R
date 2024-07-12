# Assuming, true parameter value, mu=1
# Observed data
y <- rnorm(10,1,2)
y
#Analytical posterior
sigma = 2 # Known standard devation of normal distribution
mu_prior = 0 # Mean of prior distribution on mu
sigma_prior = 3 # Standard deviation of prior distribution on mu
n = 10 # no. of observations
analytical_mu_post <- rnorm(10000,
                            mean=(((sigma^2)*(mu_prior))+
                                    ((sigma_prior^2)*sum(y)))/
                              (sigma^2 + (n*(sigma_prior^2))),
                            sd=(1/(sigma_prior^2))+(n/(sigma^2)))
hist(analytical_mu_post,freq = FALSE)
# Grid approximation

# Create grid points

mu_grid <- seq(-10,10,length=1000)
head(mu_grid)

#Calculate likelihood and posterior at each grid point
df.posterior <- data.frame(matrix(ncol=3,nrow=length(mu_grid)))
colnames(df.posterior) <- c("mu","likelihood","prior")
for(i in 1:length(mu_grid)){
  likelihood <- prod(dnorm(y,mu_grid[i],2))
  prior <- dnorm(mu_grid[i],0,3)
  df.posterior[i,] <- c(mu_grid[i],likelihood,prior)
}















