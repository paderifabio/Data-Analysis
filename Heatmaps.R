#### CHAPTER 10 HEATMAP: STATIC AND INTERACTIVE ####

"
le mappe di calore sono un altro modo per visualizzare i cluster

heatmaps() pacchetto stats
heatmap.2() meglio
"""

library(gplots)

order_
  

df = scale(mtcars)

require(dplyr)
glimpse(mtcars)

library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(df, scale = "none", col= col)
 #oppure
col = colorRampPalette(c("red","white","blue"))(256)

library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(df, scale = "none", col = col,
        RowSideColors = rep(c("blue", "pink"), each = 16),
        ColSideColors = c(rep("purple", 5), rep("orange", 6)))

library("gplots")
heatmap.2(df, scale = "none", col = bluered(100),
          trace = "none", density.info = "none")

library("pheatmap") 
pheatmap(df, cutree_rows = 4)

library("d3heatmap")
d3heatmap(scale(mtcars), colors = "RdYlBu",
          k_row = 4, # Number of groups in rows 
          k_col = 2) # Number of groups in columns 

# si possono anche zoommare!

library(dendextend)
# order for rows
Rowv <- mtcars %>% scale %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", 1.2) %>%
  ladderize
# Order for columns: We must transpose the data
Colv <- mtcars %>% scale %>% t %>% dist %>% hclust %>% as.dendrogram %>% set("branches_k_color", k = 2, value = c("orange", "blue")) %>% set("branches_lwd", 1.2) %>%
  ladderize

heatmap(scale(mtcars), Rowv = Rowv, Colv = Colv, scale = "none")


library(gplots)
heatmap.2(scale(mtcars), scale = "none", col = bluered(100),
          Rowv = Rowv, Colv = Colv,
          trace = "none", density.info = "none")

library("d3heatmap") 
d3heatmap(scale(mtcars), colors = "RdBu",
                               Rowv = Rowv, Colv = Colv)

