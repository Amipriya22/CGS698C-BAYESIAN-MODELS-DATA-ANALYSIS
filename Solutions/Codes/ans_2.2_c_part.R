f = function(k,lambda)
{
  (lambda^k)*(exp(-lambda))/(factorial(k))
}

lambda_value = 10
k_values = seq(from = 0, to = 50,by = 1)
pmf_values = f(k_values,lambda_value)
plot(k_values,pmf_values,col="red",
     main = "Answer 2.2(c) \nProbability Mass Function",
     xlab = "X",
     ylab = "P(X=x)",
     pch = 16,type ="p")

