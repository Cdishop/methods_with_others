

## purpose
# regress bad on stupid and examine the relationship

## steps
# load in my "cleaned.csv"
# correlate stupid and bad
# run a regression, such that bad is regressed on stupid












# load in my "cleaned.csv"

df <- read.csv('../data/cleaned.csv')




# correlate stupid and bad

cor(df$stupid, df$bad)

library(psych)
corr.test(df$stupid, df$bad)

?corr.test





# run regression
# regress bad on stupid

model1 <- lm(stupid ~ bad, data = df)
summary(model1)


# using structural equations modeling
# regress bad on stupid

library(lavaan)
sem_model_string <- "

stupid ~ bad

"
sem_model_fit <- sem(sem_model_string, data = df)
summary(sem_model_fit, fit.measures = T)


