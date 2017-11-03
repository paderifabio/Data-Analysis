#### DECISION TREES ####

form = as.formula("income ~ age + workclass + education + marital.status +
                  occupation + relationship + race +sex + capital.gain +
                  capital.loss + hours.per.week")

mod_tree = rpart(form, data = train)
library(partykit)
plot(as.party(mod_tree))
mod_tree

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

"""
come facciamo a eliminare variabili? 
di default, ogni split deve diminuire l'errore
dell'1%, vediamo quanto dimiuisce l'errore
Inoltre un altro modo per verificare l'accuratezza
del modello è guardare la matrice di confusione
"""

printcp(mod_tree)
plotcp(mod_tree)

"""
la matrice di confusione aiuta a vedere come si comporta il modello
"""

train <- train %>% 
  mutate(income_dtree = predict(mod_tree, type = "class"))

confusion = tally(income_dtree~income, data = train, format = "count")
sum(diag(confusion)) / nrow(train)

"""
miglioriamo rispetto al null model perchè abbiamo un'accuratezza 
dell' 84,4%


Scegliere i parametri

il modello precedente inserisce solo parametri
che riducono l'errore dell 1%, 
se mettiamo 0.002 cosa succede?
più parametri ma anche più accuratezza

"""
mod_tree2 = rpart(form, data = train, 
                  control = rpart.control(cp = 0.002))

library(partykit)
plot(as.party(mod_tree2))