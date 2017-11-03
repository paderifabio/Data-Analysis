#### RANDOM FORESTS ####

"""
Le random forest sono aggregazioni di decision tree in base a una regola

Si costruiscono in questo modo

1. Scegliere quanti decision tree costruire (questo comando è controllato dall'
argomento "ntree") e il numero di variabili da considerare in ogni decision tree
perchè vengono selezionate casualmente anche le variabili (argomento mtry)

2. Scegliere in modo casuale delle righe del dataframe con reinserimento(bootstrapping)

3. Selezionare in modo casuale un numero di variabili dal data frame

4. Creare il decision tree con il data set risultante

5. Ripetere la procedura un numero ntree di volte.

La predizione viene fatta prendendo la classe che in percentuale risulta più
volte in base ai decision tree costruiti

"""

library(randomForest)


mod_forest = randomForest(form, data = train, ntree = 201, mtry = 3)
mod_forest

plot(mod_forest)

sum(diag(mod_forest$confusion)) / nrow(train)

"""
siccome ogni random forest usa diversi set di variabili, è possibile tenere
traccia di quali variabili sono più influenti
questa è la nozione di importanza. Non c'è il 
p-value in questo caso. Non ci sono forme di 
inferenza statistica
"""

library(tibble)

importance(mod_forest) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(MeanDecreaseGini))

"""
le predizioni sempre con il comando predict
"""
predizioni = predict(mod_forest, newdata = test, type = "class")
mean(predizioni == test$income)


