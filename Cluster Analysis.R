### CLUSTER ANALYSIS ####

#### Calcolare la distanza tra le osservazioni ####

"""
Il primo passo della cluster analysis è scegliere come misurare la distanza
tra le osservazioni. Per questo motivo le variabili devono essere scalate
perchè così sono confrontabili.
Esistono diversi metodi per misurare la distanza tra le osservazioni. I metodi
classici sono la distanza Euclidea e la distanza di Manhattan.
Esistono altre misure di dissimilarità che si basano sulla correlazione tra
le variabili. La correlazione di Pearson è la più utilizzata ed è una tecnica
parametrica mentre la correlazione di Kendall e di Spearman non sono parametriche
e si utilizzano con variabili qualitative.
Nella maggior parte dei software la distanza di default è la distanza Euclidea
standardizzando le misure di distanza (Euclidea Manahattan Correlation e Eisen diventano simili)

"""

set.seed(123)

summary(USArrests)
# prendiamo solo 15 casi per semplicità

ss = sample(1:50, 15) 
df = USArrests[ss,] 
df.scaled = scale(df)

"""
diversi pacchetti e funzioni calcolano le distanze 
dist() del pacchetto stats accetta solo numeric come input
get_dist() del pacchetto factorextra anche, ma supporta
anche le distanze correlation based quindi Pearson, Kendall e Sperman
daisy() del pacchetto cluster consente altri tipi di variabili
il Gower coefficient è il più usato per misure di prossimità per mixed 
data types

tutte queste funzioni calcolano la distanza tra le righe dei dati
"""
#### COMPUTING EUCLIDEAN DISTANCE ####


dist.eucl = dist(df.scaled, method = "euclidean")

# per visualizzare meglio i dati
round(as.matrix(dist.eucl), 1) # queste sono le distanze tra le righe


round(as.matrix(dist.eucl)[1:3,1:3], 1)

"""
le colonne sono le variabili, per fare confronti pairwaise tra variabili bisogna
trasporre i dati! e avere le variabili nelle righe
"""

#### COMPUTING CORRELATION BASED DISTANCES ####

"""
La distanza viene costruita sottraendo 1 al coefficiente di correlazione
"""

library(factoextra)

dist.cor = get_dist(df.scaled, method = "pearson")

round(as.matrix(dist.cor)[1:3,1:3],1)

#### COMPUTING DISTANCES FOR MIXED DATA ####


"""
la funzione daisy fornisce una soluzione se ci sono variabili non numeriche,
accetta anche variabili qualitative, e ordinali.
"""

library(cluster)

data("flower")

str(flower)

dd = daisy(flower)
round(as.matrix(dd)[1:3,1:3],1)

#### VISUALIZZARE LE MATRICI DI DISTANZA #### 

# una funzione facile è questa, verrà approfondita più avanti

fviz_dist(dist.eucl)

"""
attenzione, queste distanze sono tre le osservazioni del dataframe
ovvero sono relative alle righe non alle variabili
"""


#### PARTITIONING CLUSTERING ####

"""
K-MEANS, PAM, CLARA sono metodi per classificare le osservazioni
di un data set in gruppi basate sulla similarità. gli algoritmi 
richiedono che sia individuato un numero di cluster a priori da generare

CLARA è per dataset molto grandi

stats package for computing K-means
• cluster package for computing PAM and CLARA algorithms 
• factoextra for beautiful visualization of clusters
"""
#### K-Means Clustering ####
"""
è l'algoritmo più usato per partizionare il dataset in gruppi
clusterizza in modo che ogni gruppo sia composto da oggetti simili
l'obiettivo è minimizzare la varianza intra-cluster

OGNI OSSERVAZIONE è ASSEGNATA A UN CLUSTER IN MODO CHE 
LA SOMMA DEI QUADRATI DELLE DISTANZE TRA LE OSSERVAZIONI E IL 
CENTRO DEL CLUSTER SIA MINIMA

Prima cosa, indicare il numero di cluster da generare
"""

data("USArrests")

df = scale(USArrests)

head(df ,3)

# kmeans(x deve essere numeric, centers, k o centri, iter max, nstart)

"""
COME scelgo il numero di cluster??

un grafico che plotta la somma dei quadrati degli scarti
within è usato per capire il numero migliore.
"""

library(ggplot2)

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
fviz_nbclust(df, kmeans, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)
fviz_nbclust(df, kmeans, method = "gap_stat") +
  geom_vline(xintercept = 4, linetype = 2)

"""
Il primo è un grafico che rappresenta la varianza within cluster.
diminuisce con l'aumentare del numero di cluster, ma a un certo punto 
non cambia di molto da k =  4 in poi
# scegliamo quindi 4 clusters
"""

#### COMPUTING K-MEANS CLUSTERING ####

# siccome l'algoritmo selezione all'inizio un numero random per il centroide
# conviene usare set seed


set.seed(123)
km.res = kmeans(df, 4, nstart = 25) # 4 è il numero di cluster

# nstart = 25 dice a R di provare 25 diversi punti di partenza e selezionare
# quello con la within cluster variance inferiore. più è grande più è stabile il risuktato


print(km.res)

aggregate(USArrests, by=list(cluster = km.res$cluster), mean)

# per aggiungere il cluster al data frame

dd = cbind(USArrests, cluster = km.res$cluster)

head(dd)

"""
è una buona idea plottare i cluster per osservare meglio
per vedere se la scelta dei cluster è giusta e per confrontARE diverse cluster analysis

sarebbe interessante creare uno scatter plot e colorare ogni dato con il cluster

ma le variabili sono più di due, una possibile soluzione è ridurre il numero
di dimensioni del dataset con un algoritmo di riduzione delle dimensioni come la PCA

la PCA prende le quattro variabili e ne tira fuori due soltanto

una soluzione è plottare le prime due comonenti della pca, che hanno i due autovettori più alti

la funzione fviz_cluster fa proprio questo, riduce le dimensioni e fa il grafico
"""

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse 
             star.plot = TRUE, # Add segments from centroids to items 
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

"""
k-means è veloce e semplice, ha delle debolezze però

assume una conoscenza dei dati a priori, perchè bisogna scegliere il numero di cluster

la soluzione finale è sensibile al centro del cluster scelto all'inizio

è sensibile agli outliers

se ordini diversamente i dati potresti avere una soluzione diversa dei cluster!

soluzioni:  variare il numero di cluster e il centro iniziale
e vedere la total within sum of 
square per scegliere la soluzione migliore



un alternativa è il PAM basato sul medoids
"""


#### K-medoids ####

"""
è correlato al k-means il medoids è un data point che è più al centro del 
cluster di tutti gli altri, ce n'è uno per ogni cluster ed è l'esempio 
rappresentativo dei membri di quel cluster, questa cosa può essere 
utile in alcune situazioni

è più robusto del k-means perchè utilizza come centro del 
cluster non la media ma il medoids come centro del cluster
quindi è meno sensibile agli outliers.

l'algoritmo necessita di specificare il numero di cluster
che si può stabilire tramite il metodo silhouette

l'algoritmo k-medoids più usato è il PAM (partitioning around medoids)

L'algoritmo funziona così, una volta trovati i medoids, assegna
ogni osservazione a medoide più vicino, dopodichè ogni medoide
e ogni non medoide viene scambiato e viene calcolata la funzione

la funzione calcola la somma delle dissimilarità di tutti gli oggetti 
dal medoide più vicino

l'algoritmo scambia centroide con osservazioni finchè può ridurre
la somma delle distanze delle osservazioni. quando non può più
ridurala è arrivato al minimo di dissimilarità e ha trovato il più
rappresentativo degli oggetti.

Quindi:

1. seleziona k oggetti per diventare i medoids

2. calcola la matrice di dissimilarità se non viene fornita

3. assegna ogni oggetto a un cluster

4. per ogni cluter cerca se alcuni oggetti del cluster riducono
il coefficiente totale di dissimilarità, se sì selezionano quell oggetto
e lo rendono il medodide del cluster

5. se un medoide è cambiato torna a (3). altrimenti l'algoritmo si arresta


l'algoritmo PAM lavora con la matrice di dissimilarità, e per
calcolare tale matrice può utilizzare due metriche:
1. la distanza euclidea, ovvero la radice quadrata della somma dei quadrati degli scarti
2. la distanza di Manhattan, che è la somma delle distanze assolute

Se i dati contengono OUTLIERS, la distanza di Manhattan è più robuta
mentre la dista euclidea è un po' più sensibile ai dati inusuali
"""

#### COMPUTE PAM IN R ####

data("USArrests")
df = scale(USArrests)

library("fpc","cluster")
library(factoextra)

"""
la funzione pam prende o il data frame o la matrice
di dissimilarità che è l'output della funzione
daisy o dist()

k ovvero il numero di cluster

metric ovvero euclidea o manatthan

stand, logical value se è true le variabili 
vengono standardizzate, viene ignorato quando la
x è la matrice di dissimilaritò

stimare il numero ottimale di clusters


l'idea  è to compute pam con diversi numeri di cluster
la media silohutte misura la qualità dei cluster
Il numero ottimale di cluster è quello che massimizza
la silhouette media in un range di possibili k
"""



fviz_nbclust(df, pam, method = "silhouette") + 
  theme_classic()


pam.res = pam(df, 2)
print(pam.res)

dd = cbind(USArrests, cluster = pam.res$cluster)
head(dd)

# visualizzare, anche qui riduce le dimensioni con la PCA. sceglie le due dimensioni 

fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse 
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)



" il k-medoids è un alternativa robusta al k-means, ogni cluster è 
rappresentato da un oggetto selezionato dentro il cluster che è il più
rappresentativo del cluster. l'algoritmo necessita del numero di cluster
per data set molto grandi il pam richiede troppa memoria e tempo di 
computazione. "


#### Clara - Clustering Large Applications ####

set.seed(1234)

df = rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300, 50, 8), rnorm(300,50,8)))

colnames(df) = c("x","y")

rownames(df) = paste0("S", 1:nrow(df))

head(df, 5)

# clara() è la funzione, ammette NAs

# numero ottimale di cluster

#### COMPUTE CLARA IN R ####

library(cluster)
library(factoextra)

fviz_nbclust(df, clara, method = "silhouette") + 
  theme_classic()


clara.res = clara(df, 2, samples = 50, pamLike = TRUE)

print(clara.res)

dd = cbind(df, cluster = clara.res$cluster)

fviz_cluster(clara.res,
             palette = c("#00AFBB", "#FC4E07"),
             ellipse.type = "t",
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())









































