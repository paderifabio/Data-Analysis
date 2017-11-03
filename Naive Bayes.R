#### NAIVE BAYES ####

"""
Un altro relativamente semplice classificatore è il Naive Bayes.
La probabilità di y|x è la probabilità di xy fratto la probabilità di x.
Supponiamo di avere una risposta binaria y e vogliamo classificare una nuova
osservazione x (che è un vettore che contiene i valori sulle scale).
se p(y = 1)|x > p(y = 0)|x ovvero la p che y sia 1 condizionata a x è maggiore della p che 
y sia 0 condizionata a x allora abbiamo evidenze che la nuova osservazione x sarà
1. Questo è il punto cruciale del Naive Bayes. In pratica, come arriviamo a calcolare
la probabilità che y sia 1? Ci arriviamo con il teorema di Bayes sulla probabilità
condizionata che viene testato sui training data (X,y)
Consideriamo la prima persona del training set. Questa persona ha 39 anni è bianco con una
laurea bachelor, lavoara per lo stato in ruolo clericale. Guadagna meno di 50k
"""

head(train, 1)

"""
il Naive Bayes fa una predizione su questa persona basandosi sulla probabilità
osservata nei dati. per esempio, in questo caso la probabilità di essere un maschio se
guadagni più di 50k è 0.845, mentre la probabilità di essere un maschio è 0.670.
la probabilità di guadagnare piu di 50k è 0.243. La regola di Bayes dice che la 
probabilità risultatnte di avere piu di 50k essendo maschio è 

Pr(>50k|male) = Pr(male|>50k) · Pr(>50k)/ Pr(male) = (0.845 · 0.243)/0.670 = 0.306

La probabilità di y condizionata a x è data dalla probabilità che si verifichi y per
la probabilità che si verifichi x se si è verificato y fratto la probabilità
che si sia verificato x
"""

library(png)
img = readPNG("~/Desktop/Data Science/R/Immagini/bayes-roule.png", native = TRUE)
grid::grid.raster(img)

"""
Questo è il caso con una singola variabile indipendente, ma il Naive Bayes può
essere esterso a molteplici variabili facendo una a volte troppo semplice
assunzione che le variabili indipendenti siano indipenenti (per questo motivo
viene detto naive)
Il Naive Bayes classifier è presente nel pacchetto e1071
"""

library(e1071)
mod_nb <- naiveBayes(form, data = train)

income_nb <- predict(mod_nb, newdata = test)

confusion <- tally(income_nb ~ income, data = test, format = "count")

confusion


sum(diag(confusion)) / nrow(test)