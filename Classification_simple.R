library(ISLR)
data("Smarket")
head(Smarket)
pairs(Smarket,col=Smarket$Direction)

# LOGISTIC REGRESSION
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Smarket, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
head(glm.probs) 
glm.probs1 = ifelse(glm.probs>0.5,"Up","Down")
head(glm.probs1)

attach(Smarket)
table(glm.probs1,Direction)
mean(glm.probs1 == Direction)
mean(glm.probs1 != Direction)

# PREDIZIONI CON LOGISTIC REGRESSION
train = Year < 2005 
test = !train
glm.fit1 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, Smarket, family=binomial, 
               subset= train)

glm.probs = predict(glm.fit1,newdata = Smarket[test,], type = "response")
glm.probs1 = ifelse(glm.probs>0.5,"Up","Down")
DirectionTest = Smarket$Direction[test] 
table(glm.probs1, DirectionTest)
mean(glm.probs1 == DirectionTest) 
mean(glm.probs1 != DirectionTest) 


# LINEAR DISCRIMINANT ANALYSIS

library(MASS)
lda.fit <- lda(Direction ~ Lag1+Lag2, subset = train)
summary(lda.fit) 
lda.fit
lda.predict = predict(lda.fit, newdata = Smarket[test,])
head(data.frame(lda.predict))
table(lda.predict$class, DirectionTest) 
table(glm.probs1, DirectionTest)
mean(lda.predict$class == DirectionTest)
mean(glm.probs1 == DirectionTest)



# QUADRATIC DISCRIMINANT ANALYSIS

qda.fit <- qda(Direction ~ Lag1+Lag2, subset = train)
summary(qda.fit) 
qda.fit
qda.predict = predict(qda.fit, newdata = Smarket[test,])
head(data.frame(qda.predict))
table(qda.predict$class, DirectionTest)
mean(qda.predict$class == DirectionTest)
mean(qda.predict$class != DirectionTest)

# K NEAREST NEIGHTBOURS

library(class)
?knn
attach(Smarket) 
Xlag = cbind(Lag1, Lag2) 
Xlag
knn.pred <- knn(Xlag[train,],Xlag[test,], Direction[train], k=1, prob = FALSE)
table(knn.pred, DirectionTest)
mean(knn.pred == DirectionTest)
mean(knn.pred != DirectionTest)
