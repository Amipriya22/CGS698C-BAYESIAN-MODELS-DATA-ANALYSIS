#Answer 4.1 part (b)----
library(ggplot2)
f = function(x,mu)
{
  exp(-(((log(x)-mu)^2)*0.5))/(x*sqrt(2*pi))
}

x_values = c(303.25,443,220,560,880);
mu_values = seq(from = 0,to = 10,by = 0.05 );

likelihood = data.frame(mu = mu_values);
#View(likelihood);
likelihood$lkl = NA;
for(i in 1:length(mu_values))
{
  likelihood$lkl[i] = prod(f(x_values,likelihood$mu[i]));
}

ggplot(likelihood,aes(mu,lkl))+
  geom_line(size = 1,colour = 'red')+
  theme_bw()+
  labs(title = "Answer 4.1 Part(b)\nLikelihood function when x is observed sample of recognition times",
       x = expression(mu),
       y = "Likelihood Function")
#View(likelihood);
