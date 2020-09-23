library(tscount)
library(tidyverse)

#ts <- ecoli$cases
ts <- influenza$cases

fit<- tsglm(ts=ecoli$cases, link="identity",
      model=list(past_obs=c(1), past_mean = c(1)), distr="poisson")

fit$coefficients

ingarch_filter <- function(ts,omega,alpha,beta){
  mean = c(ts[1])
  
  for(time in 2:length(ts)){
    mean = c(mean, omega+alpha*ts[time]+beta*mean[time-1])
  }
  data.frame(values = ts,
             filter = mean)
}

fit_df <- ingarch_filter(ts, fit$coefficients[[1]], 
                         fit$coefficients[[2]],
                         fit$coefficients[[3]]
                         )

#plot(fit_df)

fit_df %>% 
  filter(values < 1550) %>% 
  plot()

ols<- fit_df %>% 
  lm(values ~ filter+0, .)

ols$coefficients

fit_df$residuals = ols$residuals

plot(fit_df$filter, fit_df$residuals)

fit_df %>% 
  filter(values <300) %>% 
  select('filter', 'residuals') %>% 
  plot()

fit_df %>% 
  filter(values <100) %>% 
  select('filter','values') %>% 
  plot()
