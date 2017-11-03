#### Statistical Learning and Predictive Analytics ####

"""
Machine Learning: una serie di algoritmi e tecniche per estrarre
informazioni dai dati senza l'intervento umano.

Supervised Learning = Creare un modello su una specifica variabile risposta in funzione
di una serie di variabili esplicative

Unsupervised Learning = approccio per individuare pattern o raggruppamenti di dati
dove non c'è una chiara variabile risposta
"""

require(rpart)
require(mdsr)
require(dplyr)
require(ggplot2)
require(plotly)
require(ggthemes)
require(partykit)

#### Data Collection and Management #### 

"""
In questo esempio si lavora con decision tree che sono recursive partitioning
La partizione dei decision trees funziona grazie all'algoritmo di Hunt, che è recursivo
D = (y, X) è un set of records associati ad un noto t e che {y1,y2} sono le etichette
della classe della variabile risposta
Se tutti i record in D appartengono ad una singola classe, allora t è una foglia chiamata
y1,
Altrimenti splitta il record in ancora due piccoli nodi, in questo modo
la purezza del nuovo nodo supera un certo threshold. ci sono diversi modi per trovare
l'ottimazle partizione.
Come si misura la purezza di una serie di record? due metodi misurano questa cosa e sono
il coefficiente di Gini e l'information gain. Tutti e due sono implementati nel pacchetto
rpart(), che usa come misura di default il Gini coefficient. 
"""


census = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = FALSE)

names(census) <- c("age", "workclass", "fnlwgt", "education",
                   "education.num", "marital.status", "occupation", "relationship",
                   "race", "sex", "capital.gain", "capital.loss", "hours.per.week",
                   "native.country", "income")

head(census)

# carina la versione con plotly
ggplotly(ggplot(census, aes(x = education, fill = sex)) +
           geom_bar(position = "dodge") + theme_few())

glimpse(census)

set.seed(364)
n = nrow(census)
test_idx = sample.int(n, size = round(0.2 * n)) # campione casuale da n grande il 20%
train = census[-test_idx, ]
test = census[test_idx,]

tally(~income, data = train, format = "percent")

"""
dato che solo il 24% guadagna più di 50K, l'accuratezza
del null model è del 76%, ovvero se predico che guadagnano
tutti meno di 50K, nel 76% dei casi indovino!
"""

library(rpart)
library(rpart.plot)
rpart(income ~ capital.gain, data = train)

train = train %>% mutate(hi_cap_gains = capital.gain >= split)

ggplot(train, aes (x = capital.gain, y = income)) +
  geom_count(aes(color = hi_cap_gains), position = position_jitter(width = 0, height = 0.1), alpha = 0.5) +
  geom_vline(xintercept = split, color = "dodgerblue", lty = 2) +
  scale_x_log10(labels = scales::dollar)

#### Decision Trees ####

form = as.formula("income ~ age + workclass + education + marital.status +
                  occupation + relationship + race +sex + capital.gain +
                  capital.loss + hours.per.week")

mod_tree = rpart(form, data = train)
mod_tree
install.packages("rattle", repos="http://rattle.togaware.com", type="source")

train = train %>% 
  mutate(husband_or_wife = relationship %in% c("Husband", "Wife"),
         college_degree = husband_or_wife & education %in% c("Bachelors", "Doctorate", "Masters", "Prof-school"),
         income_dtree = predict(mod_tree, type = "class"))

cg_splits = data.frame(husband_or_wife = c(TRUE,FALSE), vals = c(5059.5,7073.5))

ggplot(train, aes(x = capital.gain, y = income)) + 
  geom_count(aes(color = income_dtree, shape = college_degree), position = position_jitter(width = 0, height = 0.1),alpha = 0.5) +
  facet_wrap(~husband_or_wife) + 
  geom_vline(data = cg_splits, aes(xintercept = vals),
             color = "dodgerblue", lty = 2) +
  scale_x_log10()

# come facciamo a eliminare variabili? di default, ogni split deve diminuire l'errore dell'1%

printcp(mod_tree)
plotcp(mod_tree)

table(train$income, train$income_dtree)

# la matrice di confusione aiuta a vedere come si comporta il modello
confusion = tally(income_dtree~income, data = train, format = "count")

sum(diag(confusion)) / nrow(train) # percetuale di classificazioni corrette

# miglioriamo rispetto al null model!!

## Scegliere i parametri ##

# il modello precedente inserisce solo parametri che riducono l'errore dell 1%, se mettiamo 0.002 cosa succede?
# più parametri ma anche più accuratezza

mod_tree2 = rpart(form, data = train, control = rpart.control(cp = 0.002))

predizioni = predict(mod_tree2, newdata = test, type = "class")

library(partykit)
plot(as.party(mod_tree))


#### Random Forest ####

## aggregazioni di decision tree in base a una regola

require(randomForest)
require(tibble)

mod_forest = randomForest(form, data = train, ntree = 201, mtry = 3)
mod_forest

plot(mod_forest)

sum(diag(mod_forest$confusion)) / nrow(train)

# siccome ogni random forest usa diversi set di variabili, è possibile tenere
# traccia di quali variabili sono più influenti
# questa è la nozione di importanza.

importance(mod_forest) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(MeanDecreaseGini))

predizioni = predict(mod_forest, newdata = test, type = "class")

mean(predizioni == test$income)

#### Nearest Neighbor #### 

## fa parte dei lazy learners, che fanno predizioni senza costruire un modello
## k-nearest neighbor

require(class)
# distanza euclidea solo con variabili quantitative

train_q = train %>%
  select(age, education.num,capital.gain,capital.loss,hours.per.week)
income_knn = knn(train_q, test = train_q, cl = train$income, k = 10)

confusion = tally(income_knn ~ income, data = train, format = "count")
confusion

mean(income_knn == train$income)


knn_error_rate <- function(x, y, numNeighbors, z = x) {
  y_hat <- knn(train = x, test = z, cl = y, k = numNeighbors) 
  return(sum(y_hat != y) / nrow(x))
}
ks <- c(1:15, 20, 30, 40, 50)
train_rates <- sapply(ks, FUN = knn_error_rate, x = train_q, y = train$income)
knn_error_rates <- data.frame(k = ks, train_rate = train_rates)
ggplot(data = knn_error_rates, aes(x = k, y = train_rate)) +
      geom_point() + geom_line() + ylab("Misclassification Rate")


#### Naive Bayes ####

library(e1071)
mod_nb <- naiveBayes(form, data = train)
income_nb <- predict(mod_nb, newdata = test)
confusion <- tally(income_nb ~ income, data = test, format = "count")
confusion


sum(diag(confusion)) / nrow(test)

#### Neural Networks ####

library(nnet)
mod_nn <- nnet(form, data = train, size = 5)

income_nn = predict(mod_nn, newdata = test, type = "class")
confusion = tally(income_nn ~ income, data = test, format = "count")

confusion

sum(diag(confusion)) / nrow(test)


#### Extende example ####


library(NHANES)
library(dplyr)
library(rpart)

people = NHANES %>%
  select(Age, Gender, Diabetes, BMI, HHIncome, PhysActive) %>%
  na.omit()

glimpse(people)

table(people$Diabetes) # oppure

tally(~Diabetes, data = people, format = "percent")

whoIsDiabetic = rpart(Diabetes ~ Age + BMI + Gender + PhysActive, 
                      data = people, contro = rpart.control(cp = 0.005, minbucket = 30))

whoIsDiabetic

library(partykit)

plot(as.party(whoIsDiabetic))

ggplot(data = people, aes(x = Age, y = BMI)) +
  geom_count(aes(color = Diabetes), alpha = 0.5) +
  geom_vline(xintercept = 52.5) +
  geom_segment(x = 52.5, xend = 100, y = 39.985, yend = 39.985) +
  geom_segment(x = 67.5, xend = 67.5, y = 39.985, yend = Inf) +
  geom_segment(x = 60.5, xend = 60.5, y = 39.985, yend = Inf) +
  annotate("rect", xmin = 60.5, xmax = 67.5, ymin = 39.985,
           ymax = Inf, fill = "blue", alpha = 0.1)

# in questo modo abbiamo disegnato il modello!!


ages <- range(~Age, data = people)
bmis <- range(~ BMI, data = people)
res <- 100
fake_grid <- expand.grid(
  Age = seq(from = ages[1], to = ages[2], length.out = res),
  BMI = seq(from = bmis[1], to = bmis[2], length.out = res))

form <- as.formula("Diabetes ~ Age + BMI")
dmod_tree <- rpart(form, data = people,
                   control = rpart.control(cp = 0.005, minbucket = 30))
dmod_forest <- randomForest(form, data = people, ntree = 201, mtry = 3)
dmod_nnet <- nnet(form, data = people, size = 6)


dmod_nb <- naiveBayes(form, data = people)

pred_tree <- predict(dmod_tree, newdata = fake_grid)[, "Yes"]
pred_forest <- predict(dmod_forest, newdata = fake_grid,
                       type = "prob")[, "Yes"]
pred_knn <- people %>%
  select(Age, BMI) %>%
  knn(test = select(fake_grid, Age, BMI), cl = people$Diabetes, k = 5) %>%
  as.numeric() - 1
pred_nnet <- predict(dmod_nnet, newdata = fake_grid, type = "raw") %>%
  as.numeric()
pred_nb <- predict(dmod_nb, newdata = fake_grid, type = "raw")[, "Yes"]

p <- tally(~ Diabetes, data = people, format = "proportion")["Yes"]


res <- fake_grid %>%
  mutate(
    "Null" = rep(p, nrow(fake_grid)), "Decision Tree" = pred_tree,
    "Random Forest" = pred_forest, "k-Nearest Neighbor" = pred_knn,
    "Neural Network" = pred_nnet, "Naive Bayes" = pred_nb) %>%
  gather(key = "model", value = "y_hat", -Age, -BMI)


ggplot(data = res, aes(x = Age, y = BMI)) +
  geom_tile(aes(fill = y_hat), color = NA) +
  geom_count(aes(color = Diabetes), alpha = 0.4, data = people) +
  scale_fill_gradient(low = "white", high = "dodgerblue") +
  scale_color_manual(values = c("gray", "gold")) +
  scale_size(range = c(0, 2)) +
  scale_x_continuous(expand = c(0.02,0)) +
  scale_y_continuous(expand = c(0.02,0)) +
  facet_wrap(~model)






