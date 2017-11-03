# here you find a very easy example of mediation analysis with Bootstrap
# and an example of Baron and Kenny method
install.packages('QuantPsyc')
library(QuantPsyc)
install.packages('mediation')
library(mediation)

data <- as.data.frame(data)

head(data)
str(data)


first.model <- lm(mediatior ~ predictor, data = data)
summary(first.model)

second.model <- lm(outcome ~ mediator + predictor, data=data)
summary(second.model)

mediation.model <- mediate(first.model,second.model, boot = TRUE, treat = "predictor", 
                           mediator = "mediator", sims = 2000)
summary(mediation.model)

plot(mediation.model, col = "blue", main = "Confidence Intervals Plot")
# ACME = average casual mediation effect
# ADE = average direct effect




# This represents the Baron and Kenny approach 

c <- lm(outcome ~ predictor, data = data) # is there an effect that could be mediated?
a <- lm(mediator ~ predictor, data = data) # is there a link between predictor and mediator?
b <- lm(outcome ~ predictor + mediator, data = data) # predictor effect must be less than before

summary(c)
summary(a)
summary(b)

# here you have an easy way to compute Betas
lm.beta(a)
lm.beta(c)
lm.beta(b)
lm.beta(mediation.model$model.y)