
# dgp

people <- 150
time <- 15
datastore <- matrix(, nrow = people*time, ncol = 5)
count <- 0

for(i in 1:people){
  
  for(t in 1:time){
    count <- count + 1
    
    if(t == 1){
      datastore[count, 1] <- i
      datastore[count, 2] <- t
      datastore[count, 3] <- rnorm(1, 0, 1)
      datastore[count, 4] <- rnorm(1, 0, 1)
      datastore[count, 5] <- rnorm(1, 0, 1)
    }else{
      
      
      datastore[count, 1] <- i
      datastore[count, 2] <- t
      datastore[count, 3] <- 0.7*datastore[count - 1, 3] + rnorm(1, 0, 1)
      datastore[count, 4] <- 0.4*datastore[count - 1, 4] - 0.3*datastore[count, 3] + rnorm(1, 0, 1)
      datastore[count, 5] <- 0.6*datastore[count - 1, 5] + 0.4*datastore[count, 4] + rnorm(1, 0, 1)
      
      
    }
    
  }
  
  
  
}


# observed

df <- data.frame(datastore)
names(df) <- c('id', 'time', 'x', 'm', 'y')

## mediation: x => m => y
## mediation: alert => performance => joy




# reform data to support 1-1-1 mediation in nlme
# double-entry (stacked) data

library(tidyverse)
dfstack <- df %>% mutate(m2 = m)
dfstack <- dfstack %>% pivot_longer(cols = c('m', 'y'),
                               names_to = 'dv',
                               values_to = 'z')


# dummy variables to indicate DV (m or y)

dfstack <- dfstack %>% mutate(
  dy = ifelse(dv == 'y', 1, 0),
  dm = ifelse(dv == 'm', 1, 0),
  dvnum = ifelse(dv == 'm', 1, 0)
)


# estimate mediation model

library(nlme)
mod1 <- lme(
  fixed = z ~ -1 + dm + dm:x + dy + dy:x + dy:m2,
  random = ~ -1 + dm:x + dy:m2 + dy:x | id,
  weights = varIdent(form = ~ 1 | dvnum),
  data = dfstack,
  control = lmeControl(opt = "optim")
)

summary(mod1)
# effect of x on m should be -0.3
# effect of m on y should be 0.4
fixef(mod1)





# more...
# https://thechangelab.stanford.edu/intensive-longitudinal-analysis/t-specifying-1-1-1-mediation-models-r/