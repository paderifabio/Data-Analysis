#### NEAREST NEIGHTBOR #### 

"""
fa parte dei lazy learners, che fanno predizioni senza costruire un modello

k-nearest neighbor considera le osservazioni che sono più vicine come più proba
bilmente aventi lo stesso outcome

Consideriamo un training set con (X, y). Per un intero positivo numero di K
il classificatore assegna la nuova osservazione x in questo modo:

1. trova le k osservazioni del training sett che sono più vicine a x, in base 
ad una determinata metrica di distanza (generalmente la distanza Euclidea).
Ipotiziamo che queste osservazioni siano D(x) C (X,y) ovvero le osservazioni più vicine

2. Per una certa funzione f, eseguire f(y) sui k valori di y nelle osservazioni D
e assegna il valore y come valore predetto associato a x. La logica è che dato che
x è simile alle altre osservazioni, il valore atteso di y per quelle osservazioni
sarà il valore di x più probabile. Prendere il valore più comune tra le osservazioni
di D è sufficiente ad assegnare alla nuova x una y.

un semplice k-NN classifier è nell pacchetto class con la funzione knn()
Consideriamo che, siccome la distanza euclidea si può eseguire solo per variabili quantitative,
dobbiamo escludere dal dataset le variabili non quantitative
"""


library(class)


train_q = train %>%
  select(age, education.num,capital.gain,capital.loss,hours.per.week)

income_knn = knn(train_q, test = train_q, cl = train$income, k = 10)

"""
il valore di k è importante perchè ci dice quante osservazioni vicine dobbiamo prendere per
decidere la classificazione
"""

confusion = tally(income_knn ~ income, data = train, format = "count")
confusion

mean(income_knn == train$income)

"""
Ci sono diversi problemi associati al knn, la cross validation può aiutare
"""


knn_error_rate <- function(x, y, numNeighbors, z = x) {
  y_hat <- knn(train = x, test = z, cl = y, k = numNeighbors) 
  return(sum(y_hat != y) / nrow(x))
}
ks <- c(1:15, 20, 30, 40, 50)

train_rates <- sapply(ks, FUN = knn_error_rate, x = train_q, y = train$income)

knn_error_rates <- data.frame(k = ks, train_rate = train_rates)

ggplot(data = knn_error_rates, aes(x = k, y = train_rate)) +
  geom_point() + geom_line() + ylab("Misclassification Rate")



