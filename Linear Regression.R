par(mfrow= c(1,1)) #nel caso in cui il garfico fosse diviso in più parti
install.packages("ISLR")
library(MASS)
library(ISLR)
?Boston
# la prima cosa da fare è fare un plot con alcune delle variabili per capirci qualcosa in più dei dati
data(Boston)
plot(medv ~ lstat,data = Boston) # ~ si legge versus, si vede così come si distribuiscono 
# i punteggi di medv versus lstat, indichiamo poi se non abbiamo attach i dati ovvero Boston
# facciamo una regressione lineare semplice adesso

ggplot(Boston, aes(medv, lstat)) + geom_point() + geom_smooth(method=lm) # linea
ggplot(Boston, aes(medv, lstat)) + geom_point() + stat_smooth() # smooth, curva
ggplot(Boston, aes(medv, lstat)) + geom_point() + stat_smooth() # il girgio intorno alla curva
ggsave(file = "medv vs lstat.pdf", width=16, height=4) # salvare il grafico come pdf

mydata <- ggplot(Boston, aes(medv)) +facet_grid(.~zn)

mytheme <- theme(panel.background = element_rect(fill='lightblue', colour='darkgrey'))
# imposto dei temi e delle preferenze, le salvo in oggetti per richiamrle quando voglio
mychart <- geom_bar(position="dodge", fill="thistle", color="black")

mydata+mytheme+mychart

# rappresenta l'intervallo di confidenza 


fit1 = lm(medv~lstat, Boston)


summary(fit1)
# adesso disegniamo la linea di regressione
abline(fit1, col="red") #come si può vedere la relazione non sembra lineare..
# capiamo cosa c'è in fit1 model
names(fit1)
# intervallo di confidenza dei coefficienti
confint(fit1)
#l'intervallo di confidenza si può anche cambiare
confint(fit1,level= 0.99)
# una funzione che si può usare è predict, che si usa per fare predizioni
predict(fit1, data.frame(lstat = c(5,10,15))) 
predict(fit1, Boston) # restituisce medv per ogni valore lstat del data frame boston (not sure)
# in questo caso i dati su cui fare le predeizioni
#li ho inseriti io, ma se si ha un data frame gia fatto o un test set si inserisce quello
#restituisce il livello di medv per valori di lstat = 5,10,15 

#si può inserire anche l'intervallo di confidenza

predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence", level = 0.99)
#restituisce il confine superiore e inferiore con un intervallo di conf del 99%
# vuol dire che siamo sicuri al 99% che quel valore ricade tra il lwr e l'upr

#la regressione lineare semplice non è molto usata, aggiungiamo variabili

fit2 = lm(medv~lstat+age, Boston)
summary(fit2)

# se vogliamo aggiungere tutte le variabili nel modello si aggiunge un punto dopo il tilde
fit3 = lm(medv~., Boston)
summary(fit3)
# age e indus non sono significativi quindi li togliamo inserndo il meno
fit4 = lm(medv~.-age-indus, Boston)
summary(fit4)

# aggiungere interazioni? fit2 = lm(medv~lstat+age, Boston)
fit5 = lm(medv~ lstat*age, Boston)
summary(fit5)

#trasformazioni non lineari, come si vede da
plot(medv ~ lstat,data = Boston)
# la relazione non sembra molto lineare
# questi sono due modi per stimare una relazione di tipo quadratico
fit6 = lm(medv~lstat+I(lstat^2), Boston)
summary(fit6)
fit7 = lm(medv~ poly(lstat, degree=5), Boston)
summary(fit7)
# in teoria vuol dire medv = lstat fino alla quinta potenza

# come gestire i dati qualitativi? intanto le variabili devono essere dummy, per ogni variabile
# categoriale dobbiamo avere n-1 variabili dummy
# prendiamo il Carseat dataset
?Carseats
# in R, se la variabile categoriale è già as.factor, lui fa da solo nella regressione,
# non cè bisogno di renderla dummy. Deve essere as.factor però
summary(Carseats)

fi8 = lm(Sales ~ .,Carseats)
summary(fi8)

fit9 = lm(Sales ~ .+Income*Advertising+Age*Price,Carseats)
summary(fit9)

# i modelli lineari come questo sono abbastanza semplici, attenzione quando si fanno 
#predizioni 
predict(fit1, data.frame(lstat = c(5,10,15)), interval = "confidence", level = 0.99)
# è importante inserire bene i dati del nostro test set

#fine