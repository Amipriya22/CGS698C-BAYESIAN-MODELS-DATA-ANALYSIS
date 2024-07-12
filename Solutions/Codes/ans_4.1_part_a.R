#Answer 4.1 part (a)----

fu = function(x,mu)
{
  exp(-(((log(x)-mu)^2)*0.5))/(x*sqrt(2*pi))
}
x_value = 220
mu_values = seq(from = 0,to = 10,by = 0.01 )
fu_values = fu(x_value,mu_values)
plot(mu_values,fu_values,col = "red",pch = 16,
     main = "Likelihood Function",
     xlab = expression(mu),
     ylab = "f(x=220,mu)",type = 'l')


