
#Fitting Distributions

library('MASS')
library('fitdistrplus')

x <- c(108487,105813,91310,109574,108062,196980,332316,317427,438818,445225,454319,498635,613234,493964,504306,468202,350491,186003,196701,266114,269342,253996,155686,121474)
x <- c(72142,97791,64198,71414,99343,127632,204672,255081,184360,199354,239969,233433,189133,229391,246459,185414,204368,201111,241180,231324,224311,172415,133029,113525)

fitdistr(x, 't')$loglik
fitdistr(x, 'normal')$loglik
fitdistr(x, 'logistic')$loglik
fitdistr(x, 'weibull')$loglik
fitdistr(x, 'gamma')$loglik
fitdistr(x, 'lognormal')$loglik
fitdistr(x, 'exponential')$loglik
fitdistr(x, 'splitnormal')$loglik

