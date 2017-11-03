#### CHAPTER 7 HIERARCHICAL CLUSTERING ####

"
è un altro tipo di partizionamento, ma non richiede di pre-specificare
il numero di cluster.

Si suddividono in due tipi.

Agglomerative Clustering in cui ogni osservazione è inizialmente 
considerata come un singolo cluster. Poi, i cluster più simili sono
successivamente aggregati fino a che non c'è un singolo grande cluster.

Divise clustering, un modo inverso rispetto all'agglomerativo. comincia
con il root, ovvero il grande cluster in cui tutti gli oggetti del 
dataset sono inclusi. poi i più eterogenei cluster sono successivamente
divisi finchè ogni osseravzione è nel suo cluster.

Il risultato di un clustering gerarchico è una rappresentazione dei dati
ad albero, chiamata anche DENDOGRAMMA

Questo tipo di rappresentazione consente di generare diversi gruppi di oggetti
che si possono poi scegliere
"

#### Agglomerative Clustering ####

"
è metodo di clustering gerarchico più utilizzato, per suddividere
le osservazioni in cluster in base alle somiglianze.

è anche chiamato AGNES ovvero agglomerative nesting.

ogni oggetto è considerato un cluster a sè, dopodichè coppie di cluster 
sono unite finchè tutti gli oggetti vengono fusi in un unico grande
cluster che contiene tutti gli oggetti.


L'algoritmo funziona così. ogni osservazione è presa singolarmente
e costituisce una FOGLIA, leaf. poi coppie di foglie più simili vengono
combinate in un unico cluster chiamato NODO. Questa procedura viene 
iterata finchè tutti i punti sono membri di un unico grande cluster
ovvero il ROOT, LA RADICE.'

L'inverso dell'agglomerative è il divise clustering chiamato anche
DIANA (divise analysis) e lavora invece che bottom up, top down.
Comincia con la radice e poi divide in due cluster in base all'
eterogeneità. questo processo viene iterato finchè ogni
osservazione è diventata un cluster'

AGNES è buono ad individuare piccoli cluster mentre DIANA a trovare
grandi cluster. 

AGNES: prepara dati, calcola la dissimilaritàs tra ogni paio di 
osservazioni. unisce le osservazioni simili,
determina dove deve essere tagliato l'albero in cluster'


i dati vanno organizzati in una matrice numerica
i dati vanno standardizzati per poter essere confrontati 
"

data("USArrests")

df = scale(USArrests)

head(df)

# compute dissimilarity matrix

res.dist = dist(x = df, method = "euclidean")

as.matrix(res.dist)[1:6, 1:6]

"""
linkage -> prende la matrice di distanza e raggruppa le
osservazioni in cluster basandosi sulla similarita
"""

res.hc = hclust(d = res.dist, method = "ward.D2")
res.hc2 = hclust(d = res.dist, method = "average")

"""
il metodo è il modo per calcolare la distanza tra due cluster. i metodi più usati
sono nel libro descritti il metodo di WArd che minimizza la varianza between cluster.
Complete e Ward sono i preferiti. WardD2 fa le distanze al quadrato

# per disegnare il dendrogramma c'è anche la funzione base di R
"""
plot(res.hc)

# oppure quella di factoextra

library(factoextra)
fviz_dend(res.hc, cex = 0.5)

"""
per interpretarlo, maggiore è l'altezza più i cluster che vengono fusi sono diversi

l'altezza è detta cophenetic distance' distanza cofenetica

Per verificare il dendogramma si può confrontare la distanza cofenetica con l'originale
matrice di dissimilarità
più la correlazione è alta, piu il dendogramma rappresenta la distanza tra oggetti'
"""

res.coph = cophenetic(res.hc)
res.coph2 = cophenetic(res.hc2)
cor(res.dist, res.coph)
cor(res.dist, res.coph2)

"""
Il secondo metodo è migliore

# con il metodo avergae per la suddivisione in
cluster la correlazione con la matrice
delle distanze euclidee è maggiore
"""

#### Cut the dendogramm into different groups ####

"""
il problema con hclustering è che non ti dice quanti cluster e dove tagliare l'albero
cut into 4 group
"""

grp = cutree(res.hc, k = 4)
head(grp, n = 4)

table(grp)

rownames(df)[grp == 1]

# Visualizzazione


fviz_dend(res.hc, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))


fviz_dend(res.hc, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE) # se voglio dei rettangoli a tagliare

# anche in uno scatterplot con la PCA

fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE,
             ggtheme = theme_minimal())

# il pacchetto cluster permette di fare tutto piu veloce

library(cluster)
res.agnes = agnes(x = USArrests, # data matrix
                  stand = TRUE, # standardizza
                  metric = "euclidean", # metrica per calcolare le distanze
                  method = "ward") # metodo di linkage

fviz_dend(res.agnes, cex = 0.6, k = 4)

res.diana = diana(x = USArrests,
                  stand = TRUE,
                  metric = "euclidean")

fviz_dend(res.diana, cex = 0.6, k = 4)

#### CHAPTER 8 COMPARING DENDOGRAMMS ####

library(dendextend)

# tanglegram() visual comparison
# cor.dendlist() correlation matrix between dendograms

df = scale(USArrests)

# per rendere più facili i grafici lavoriamo con 10 osservaizioni

df = scale(sample_n(USArrests, 10, replace = FALSE))

# oppure

ss = sample(1:50, 10)
df = df[ss,]

# distanze
res.dist = dist(df, method = "euclidean")


# due hierarchical clustering

hc1 = hclust(res.dist, method = "average")
hc2 = hclust(res.dist, method = "ward.D2")

# create two dendograms

dend1 = as.dendrogram(hc1)
dend2 = as.dendrogram(hc2)

# create a list holding dendrogram

dend_list = dendlist(dend1, dend2)

# tanglegram li plotta insieme

tanglegram(dend1, dend2)

# si può personalizzare

tanglegram(dend1, dend2, 
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = FALSE,
           common_subtrees_color_branches = TRUE,
           main = paste("entanglement", round(entanglement(dend1, dend2),  2)))

# la funzione seguente serve per fare la matrice di correlazione
# tra le distanze cofenetiche tra due trees

cor.dendlist(dend_list, method = "cophenetic")

# Baker correlation

cor.dendlist(dend_list, method = "baker")

# cophenetic correlation coefficient

cor_cophenetic(dend1, dend2)

## operatore pipe


dend1 = df %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram 
dend3 <- df %>% dist %>% hclust("average") %>% as.dendrogram 
dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram

dend_list = dendlist("Complete" = dend1, "Single" = dend2,
                     "Average" = dend3, "Centroid" = dend4)

cors = cor.dendlist(dend_list)

round(cors, 2)

library(corrplot)

corrplot(cors, "pie", "lower")


#### CHAPTER 9 VISUALIZING DENDROGRAMS ####


data("USArrests")

dd = dist(scale(USArrests), method = "euclidean")
hc = hclust(dd, method = "ward.D2")

library(factoextra)
library(dendextend)

# normale
fviz_dend(hc, cex = 0.5)

# personalizzare assi
fviz_dend(hc, cex = 0.5,
          main = "Dendrograms - ward.D2",
          xlab = "Objects", ylab = "Distance",
          sub = "")

# horizontal
fviz_dend(hc, cex = 0.5, horiz = TRUE)

library(ggthemes)
# tagliare il dendogramma e colorare by groups
# è integrato con ggplot quindi si può cambiare il tema
fviz_dend(hc, k=4,
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE,
          ggtheme = theme_dendro())

# diversi colori

# horiz
fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco",
          horiz = TRUE, rect = TRUE, rect_border = "jco",
          rect_fill = TRUE)

# circular

fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco", type = "circular"
)

# phylogenic

library(igraph)

fviz_dend(hc, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)


fviz_dend(hc, k = 4,
          k_colors = "jco",
          type = "phylogenic", repel = TRUE,
          phylo_layout = "layout.auto") # vari layout

### Dendrogrammi per dataset larghi ###

# si può zoommare o disegnarlo in un pdf

fviz_dend(hc, xlim = c(1,20), ylim = c(1,8))

# disegnare un sub-albero di un dendrogramma


dend_plot = fviz_dend(hc, k = 4, # Cut in four groups
                      cex = 0.5, # label size 
                      k_colors = "jco")



dend_data = attr(dend_plot , "dendrogram") # estraggo i dati del dendrogramma

dend_cuts = cut(dend_data, h = 10)

fviz_dend(dend_cuts$upper)

print(dend_plot)

#parte 1
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")
#parte 2
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")

# se abbiamo un grande dendrogramma possiamo salvarlo
# come PDF

#### SCRIVERE UN PDF!!!!! ####
pdf("~/Desktop/dendrogram.pdf", width = 30, height = 15)
p = fviz_dend(hc, k = 4, cex =1, k_colors = "jco")
print(p)
dev.off()

## Manipulation con Dendexteng ## 

# usare l'operatore pipe

data <- scale(USArrests)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2") 
dend <- as.dendrogram(hc)
plot(dend)

library(dendextend)

dend = USArrests[1:5,] %>%
  scale %>%
  dist %>%
  hclust(method = "ward.D2") %>%
  as.dendrogram
plot(dend)
  
library(dendextend)
# 1. Create a customized dendrogram
mycols <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
dend <- as.dendrogram(hc) %>%
  set("branches_lwd", 1) %>% # Branches line width 
  set("branches_k_color", mycols, k = 4) %>% # Color branches by group 
  set("labels_colors", mycols, k = 4) %>% # Color labels by groups 
  set("labels_cex", 0.5) # Change label size

library(factoextra)
fviz_dend(dend)














