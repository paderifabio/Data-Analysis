binom.test(3,10)


# non è significativo quindi non possiamo rigettare l'ipotesi che la moneta è bilanciata.
# con 3 successi su 10 non cambia

binom.test(30,300)

# è sempre il trenta percento ma adesso è significativo. il p value dipende da n quindi se aumento n 
# sicuramente il p value scende.
# test binomiale.

# potenza del test? potenza ha senso se fai un t test ma un sem o un multivariato
# la potenza è utile come concetto teoricamente, serve a ragionare...a lato pratico non serve a molto
# serve se ci serve la dimensione campionaria
# power.t.test() su R. per avere l n mi serve la dimensione dell effetto...ma se so la dimensione dell
#effetto cosa faccio la ricerca a fare?

power.t.test(10,0.52, sd=0.61, type = 'one.sample')


# quando calcoliamo la potenza a posteriori non ci dice niente di più, perchè considera le info che abbiamo
#trovato in precedenza

#sem tools fa la power analysis sulla CFA.

library(sem)
data(Klein)
head(Klein)
?Klein

install.packages("sos")
library(sos)

findFn('meta analysis') # trova tutti i pacchetti che contengono questa parola con parola chiave

(log(x = 1.5) + 1)^2

bfi <- read_csv2(file.choose()) # ti fa scegliere l'indirizzo fisico del file!!


View(bfi)


as.data.frame(bfi)
str(bfi)

summary(bfi)

kidiq$id <- c(1: dim(kidiq)[1]) # creare la variabile id

percentmiss = function(x) { sum(is.na(x)) / length(x) * 100}
missing = apply(kidiq[ , -1], 1, percentmiss)
table(missing)
replacepeople = subset(kidiq, missing <= 5)
summary(replacepeople)
apply(replacepeople[ , -1], 2, percentmiss)

nomiss = replacepeople

##outliers
mahal = mahalanobis(nomiss[ , -1],
                    colMeans(nomiss[ , -1]),
                    cov(nomiss[ , -1]))
cutoff = qchisq(1-.001, ncol(nomiss[ , -1])) ##cutoff
cutoff 
ncol(nomiss[ , -1]) ##df
table(mahal < cutoff)
noout = subset(nomiss, mahal < cutoff)

##additivity
correl = cor(nomiss[ , -1])
symnum(correl)

##assumption set up
random = rchisq(nrow(nomiss), 7)
fake = lm(random ~., data = nomiss[ , -1])
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##Linearity
qqnorm(standardized)
abline(0,1)

##homog and s
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

m1 <- lm(kid_score ~ mom_iq + mom_age, data = kidiq)
summary(m1)
plot(m1) # indipendenza dei residui / normalità dei residui

prediction <- predict(m1, kidiq)
prediction
mean(prediction == kidiq$kid_score)
kidiq$kid_score #beta diviso standard error di beta trovo un parametro che ha la distribuzione tipo t


ggplot(kidiq, aes(kid_score, mom_iq)) + geom_point() + geom_smooth(method=lm)
library(ggplot2)
ggplot(kidiq, aes(mom_iq, kid_score)) + geom_point(aes(color=mom_hs)) + 

library(effects)
plot(allEffects(m1))

anova(m4) # analisi della covarianza ancova

plot(allEffects(m4))


anova(m1,m2,m4,m5)
# confrontiamo i modelli. RSS devianza residua
# due modelli sono nested se azzerando un parametro nel successivo ottengo quello prima
# quindi sto facendo un test del parametro
# AIC e BIc efficienza del modello nella previsione dei dati..funzioni di perdita

# il modello nullo mi da il limite inferiore ~ 1 
# A <- AIC(m1,m2,m3)
# A[order(A[,2]),]
# calcolare l'evidenza relativa...molto utile deltaAICdiviso 2 ,esponenziale

A <- AIC(m1,m2,m4)

A[order(A[,2]),]

deltaAIC <- AIC(m2)-AIC(m4)
deltaAIC

evidenzarelativa <- exp(deltaAIC/2)
evidenzarelativa #quanto è più significativo? ad esempio 31 volte più evidente dell'altra

# si può calcolare il Bayes factor
# bayes factor,,,ha vantaggi e svantaggi rapporto fra f(x|M1)/f(x|m0)

install.packages("BayesFactor")
library(BayesFactor)
generalTestBF(kid_score ~ mom_iq*mom_hs, data=kidiq) #fa il bayesfactor di tutti rispetto almodello nullo

BF <- generalTestBF(kid_score ~ mom_iq*mom_hs, data=kidiq)
plot(BF)

load(file.choose())

obsessive

plot(obsessive$values, obsessive$subj, pch=19) # variabilità individuale diversa!

m0 <- lm(values ~ 1, data = obsessive)
summary(m0) #un parametro è l'intercetta ma l'altro è LA VARIANZA DEI RESIDUI parametro
boxplot(residuals(m0)~obsessive$subj, horizontal=T) # i residui non hanno ne media 0 ne varianza costante

m1 <- lm(values ~ subj-1, data=obsessive) #togliendo l'intercetta ho le medie dei soggetti e non gli scarti dal primo soggetto che è considerato intercetta
summary(m1) #tutti stesso errore standard
boxplot(residuals(m1)~ obsessive$subj, horizontal=T) #residui media 0 ma varianza non costante
# non va bene questo modello perchè troppi parametri
# libro di bates sui mixed models

library(lme4)
mx1 <- lmer(values ~ (1|subj), data = obsessive) #blups(bates 200boh) (1|subj) è l'effetto random
#lui ha il codice del soggetto e lo usa per raggruppare 
summary(mx1)
mx1 #effetti fissi intercetta e poi effetti random soggetti e residui
fixef(mx1) # effetto fisso
ranef(mx1) # sei effetti però, ovvero lo scarto dei soggetti rispetto alla media.
#graficamente?
#sei rette che esprimono le medie dei soggetti yi = beta + sigma
coef(mx1) #modello con intercette variabili.



mx2 <- lmer(values ~ group+(1|subj), data = obsessive)
mx2
summary(mx2) #intercetta è media nel gruppo 0 mentre group1 è la differenza tra i gruppi
ranef(mx2) # adesso scarto dal suo gruppo
coef(mx2) 
# non abbiamo il p value ma abbiamo la distribuzione t
# il p value non c'è ma se è superiore a 2 va bene
curve(dt(x,2), -4,4) #-4,4 è il range, 2sono i df


anova(mx1,mx2) #likelihood ratio test! attenzione...nidificati e sempre con gli effetti random!non uno con e uno senza

#chi square significativo quindi 
#c'è differenza tra i modelli. aggiungendo il gruppo quindi differenza tra gruppi

confint(mx2)
library(lattice)
densityplot(profile(mx2))
dotplot(ranef(mx2,T))
library(effects)
plot(effect("group",mx2))

#differenziale di BIC sempre utile per capire se il modello migliora
library(BayesFactor)
generalTestBF(values ~ group+subj, data=as.data.frame(obsessive),whichRandom="subj")
plot(generalTestBF(values ~ group+subj, data=as.data.frame(obsessive),whichRandom="subj"))

