
# Build own DGPs to understand the nuance of new techniques you are learning




#
# hypothetical study



#
# hypotheses:
# workplace perceptions of justice are positively related to job satisfaction
## justice -> job sat
# workplace perceptions of social support are positively related to job satisfaction
## support -> job sat



#
# statistical model I eventually plan to estimate:

summary(lm(job_sat ~ justice + support,
           data = df))



#
# what data-generating mechanism could produce observations consistent with my planned statistical model?

people <- 500
justice <- rnorm(people, 10, 3)
support <- rnorm(people, 6, 4)
error <- rnorm(people, 0, 3)

job_sat <- 0.5*justice + 3*support + error


#
# the data I collect

df <- data.frame(
  'id' = c(1:people),
  'justice' = c(justice),
  'support' = c(support),
  'job_sat' = c(job_sat)
)


#
# run my statistical model. What should my estimates be?

summary(lm(job_sat ~ justice + support,
           data = df))










#
# what if I had fewer participants?
people <- 5
justice <- rnorm(people, 10, 3)
support <- rnorm(people, 6, 4)
error <- rnorm(people, 0, 3)
job_sat <- 0.5*justice + 3*support + error
df <- data.frame(
  'id' = c(1:people),
  'justice' = c(justice),
  'support' = c(support),
  'job_sat' = c(job_sat)
)
summary(lm(job_sat ~ justice + support,
           data = df))


#### power analysis: iterate above 1000s of times by putting it into a for-loop, adjust N each iteration




#
# what if the true dgp went in the other direction? job_sat causes social support

people <- 500
justice <- rnorm(people, 10, 3)
job_sat <- rnorm(people, 15, 4)
error <- rnorm(people, 0, 3)

support <- 0.3*justice + 2*job_sat + error

df <- data.frame(
  'id' = c(1:people),
  'justice' = c(justice),
  'support' = c(support),
  'job_sat' = c(job_sat)
)

summary(lm(job_sat ~ justice + support,
           data = df))










#
# Statistical models only have one way of viewing the data fed to them.
# They presume a DGP, which is embedded in the model equations, and then tell you...
# these estimates (b1 & b2) are the coefficients that could most easily generate the dependence structures that exist in your observed data IF THE DGP IS THE ONE I (THE STATISTICAL MODEL) ASSUME.
# A statistical model will give you an answer. But the answer is wholly incorrect if the true DGP is different from the form represented by the statistical model. 
# Evaluating fit and parameter estimates indicates the discrepancy between the model and data, but it does not reveal the discrepancy between the model and reality.

# Even more challenging: the distinguishability problem.
# Different dgps give rise to the same observed data. 

'
Dependence structures that exist
within collected data may be consistent with the functioning of many
different underlying causes. Any given pattern may correspond to
more than one generating mechanism. As a consequence, model
results (fit statistics and parameter estimates) are at times similar
across different data-generating processes. Consider three layers or
tiers: layer one contains causal processes. It contains reality or datagenerating mechanisms. These mechanisms give rise to observed
data located at layer two. At this layer, data display dependence
structures among the many or few variables that exist in the set. Statistical models and their estimates, after being fitted to the observed
data, sit at the next layer, layer three. Distinguishability is the challenge of using models located at layer three to infer processes at layer 
one while recognizing two inherent limitations: (a) different processes contained in layer one often yield the same dependence structures at layer two, 
and (b) models applied at layer three only have
access to the information contained in layer two. It is difficult to distinguish between two causal 
mechanisms at layer one that yield the
same observations at layer two using only statistical techniques at layer three.

Braitenberg (1986) called it the law of uphill analysis: discriminating among
mechanisms by witnessing observations is hard because different
mechanisms sometimes yield the same observations.
'


# Even more challenging: the same dgp may give rise to many different observations
# https://www.autodesk.com/research/publications/same-stats-different-graphs
















# latent change example



#
# dgp: some people become better leaders over time (change from wave 1 to wave 2) 
# -- assertiveness is 1 IV that makes people stronger leaders

people <- 200
assertive <- rnorm(people, 10, 2)
leader1 <- rnorm(people, 10, 5)
change <- 0.7*leader1 + 0.3*assertive + rnorm(people, 0, 3)
leader2 <- leader1 + change + rnorm(people, 0, 0.5)


#
#
# observed data

df <- data.frame(
  'id' = c(1:people),
  'l1' = c(leader1),
  'l2' = c(leader2),
  'assert' = c(assertive)
)


#
#
# statistical model


library(lavaan)
modstring <- '

# regress leadership 2 on leadership 1, with 1 as beta
l2 ~ 1*l1
# define latent change score factor
dleader =~ 1*l2
# conditional mean of change score
dleader ~ 1
# mean of leadership1
l1 ~ 1
# constrain intercept of leadership2 to 0
l2 ~ 0*1
# estimate conditional variance of change score, and of leadership1
dleader ~~ dleader
l1 ~~ l1
# fix variance of leadership2 to 0
l2 ~~ 0*l2
# self feedback parameter (sort of like autoregression), 
# the affect from assertiveness
dleader ~ b1*l1 + g1*assert

'

fitmod <- sem(modstring, data = df)
# b1 should be 0.7, g1 should be 0.3
summary(fitmod)





#
#
# do you see the learning process? 

# create my dgp
# observed data
# apply planned statistical model
# do my statistical model estimates reflect what I programmed into the dgp?








#
#
# don't do this...



#####  Simulate data for a Univariate Latent Change Score model ####

#Fix sample size
samplesize<-500

#Simulation specification
ULCS_simulate<-'

#####     The following lines specify the core assumptions of the LCS 
#####     and should not generally be modified

COG_T2 ~ 1*COG_T1     # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2     # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0  


###### The following five parameters will be estimated in the model. 
###### Values can be modified manually to examine the effect on the model

dCOG1 ~ 10*1        # This fixes the intercept of the change score to 10 
COG_T1 ~ 50*1       # This fixes the intercept of COG_T1 to 50 

dCOG1 ~~ 5*dCOG1    # This fixes the variance of the change scores to 5. 
COG_T1 ~~ 8*COG_T1  # This fixes the variance of the COG_T1 to 8. 

dCOG1~-0.1*COG_T1   # This fixes the self-feedback parameter to -0.1. 

'

#Simulate data
set.seed(1234)
simdatULCS<-simulateData(ULCS_simulate,sample.nobs = samplesize,meanstructure = T)


write.csv(simdatULCS,'1_simdatULCS.csv') #save data to be used by other programs

#Fit the Univariate Latent Change Score model to simulated data
ULCS<-'

COG_T2 ~ 1*COG_T1     # Fixed regression of COG_T2 on COG_T1
dCOG1 =~ 1*COG_T2     # Fixed regression of dCOG1 on COG_T2
COG_T2 ~ 0*1          # This line constrains the intercept of COG_T2 to 0
COG_T2 ~~ 0*COG_T2    # This fixes the variance of the COG_T2 to 0 

dCOG1 ~ 1             # This estimates the intercept of the change scores 
COG_T1 ~  1           # This estimates the intercept of COG_T1 
dCOG1 ~~  dCOG1       # This estimates the variance of the change scores 
COG_T1 ~~   COG_T1    # This estimates the variance of COG_T1 
dCOG1~COG_T1          # This estimates the self-feedback parameter

'

fitULCS <- lavaan(ULCS, data=simdatULCS, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitULCS, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

