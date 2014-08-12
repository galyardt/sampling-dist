#### Functions & Parameters for the Sampling Dist App

#############
norm.pars = list(mu=10, s = 3)
unif.pars = list(min = 0, max = 20)
chisq.pars = list(df = 12)
weib.pars = list(shape=0.8, scale=5)
#################
# to plot the population distribution
dens <- function(x, type){
  switch(type,
         norm = dnorm(x, mean=norm.pars$mu, sd=norm.pars$s),
         unif = dunif(x, min=unif.pars$min, unif.pars$max),
         chisq = dchisq(x, df = chisq.pars$df),
         weib = dweibull(x, shape = weib.pars$shape, scale = weib.pars$scale)
  )
}

#################
# to generate a single sample of data
sing.samp = function(n, type){
  switch(type,
         norm = rnorm(n, mean=norm.pars$mu, sd=norm.pars$s),
         unif = runif(n, min=unif.pars$min, unif.pars$max),
         chisq = rchisq(n, df = chisq.pars$df),
         weib = rweibull(n, shape = weib.pars$shape, scale = weib.pars$scale)
  )
}

################
## Now for the sampling distribution
########
## first specify the parameter of interest
param = function(x, type){
  switch(type, 
         mean=mean(x),
         median=median(x),
         Q1 = quantile(x, 0.25),
         Q3 = quantile(x, 0.75))
}
## now calculate the sampling dist
samp.dist = function(n, dis, stat, B=1000){
  out = rep(NA, B)
  for(b in 1:B){
    tmp = sing.samp(n, dis)
    out[b] = param(tmp, stat)
  }
  return(out)
}

