## Meta-Analysis
install.packages("effsize")
library(effsize)
install.packages(c("meta","rmeta","mvmeta"))
install.packages("forestplot")
install.packages("metafor")

## il pacchetto fornisce un insieme di funzioni per il calcolo di molti ES
## a pratire dalle principali statistiche ricavabili negli articoli: t,F chi-square, r etc

## disponiamo di medie 10 e 12 e deviazioni standard 1 e 1.3
install.packages("compute.es")
library(compute.es)

mes(10,12,1,1.3,30,30) #medie sd e numerosità

tes(3,30,30) # due valori di t e poi numerosità dei due gruppi

fes(4,30,30) # due valori di F

res(0.3, n=30)  # con la correlazione tutti gli effect size

chies(4,30) # con il chi quadrato

study = c(1,2,3,4,5,1,2,3,4,5)
m_pre = c(30.6,23.5,0.5,53.4,35.6,23.1,24.9,0.6,55.7,34.8)
m_post = c(38.5,26.8,0.7,75.9,36.0,19.7,25.3,0.6,60.7,33.4)
sd_pre = c(15.9,3.1,0.1,14.5,4.7,13.8,4.1,0.2,17.3,3.1)
sd_post = c(11.6,4.1,0.1,4.4,4.6,14.8,3.3,0.2,17.9,6.9)
ri = c(0.47,0.64,0.77,0.89,0.44,0.47,0.64,0.77,0.89,0.44)
ni = c(20,50,9,10,14,20,42,9,11,14)
group = c("T",'T','T','T','T','C','C','C','C','C')

metanalisi = cbind(study,m_pre,m_post,sd_pre,sd_post,ri,ni,group)
metanalisi = as.data.frame(metanalisi)
metanalisi

t = (m_post - m_pre)/sqrt((sd_pre^2+sd_post^2-2*sd_pre*sd_post*ri)/ni)

metanalisi = cbind(metanalisi,t)

effect = tes(t, ni, ni)

metanalisi = as.data.frame(cbind(metanalisi,effect))

View(metanalisi)

aggregate(d~group, hist, data= metanalisi)
library(ggplot2)

ggplot(metanalisi, aes(x=d)) +facet_grid(.~group) + geom_density(fill="blue")
ggplot(metanalisi, aes(x=d))  + geom_density(position="dodge", fill="blue")

ggplot(metanalisi, aes(x=U3.g)) +facet_grid(.~group) + geom_density(position="dodge", fill="blue")
ggplot(metanalisi, aes(x=U3.g))  + geom_density(position="dodge", fill="blue")

ggplot(metanalisi, aes(x=g)) +facet_grid(.~group) + geom_density(position="dodge", fill="blue")
ggplot(metanalisi, aes(x=g))  + geom_density(position="dodge", fill="blue")

ggplot(metanalisi, aes(x=fisher.z)) +facet_grid(.~group) + geom_density(position="dodge", fill="blue")
ggplot(metanalisi, aes(x=fisher.z))  + geom_density(position="dodge", fill="blue")

onet2cohend <- function(t,n) {
  d <- 2*t/(sqrt(n-2))
  return(d)
}

d = onet2cohend(t,ni)
metanalisi= cbind(metanalisi,d)
metanalisi

m0 = lm(d ~ 1, data = metanalisi)
m1 = lm(d ~ group, data = metanalisi)
m2 = lmer(d ~ group + (1|study), data = metanalisi)
m2
summary(m2)
plot(m1)
summary(m1)

anova(m0,m1)

exp((BIC(m1)-BIC(m0))/2)

x <- rnorm(10000)
hist(x)
hist(x,freq=FALSE)
curve(dnorm(x), add=TRUE)
# con la distribuzione normale approssimo distribuzioni teoriche.