#### R for Data Science Wickham ####

"
1. Importare i dati

2. Tidy data: pulirli e prepararli
ogni colonna una variabile
ogni riga un'osservazione

3. Trasformare i dati
creare nuove variabili di interesse

Tidy and Transform <- Wrangling

4. Visualization and Modeling

5. Communication

"

# Guida, Data Visualization ####

getwd()
setwd("/Users/fabiopaderi/Desktop/Data Science/R")
dir.create("R for Data Science")
setwd("./R for Data Science")

library(tidyverse)


mpg
# displ è la grandezza del motore in litri
# hwy è l'efficienza di consumo in autostrata

# geom_point -> Scatterplot

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))


"
è possibile modificare SIZE, SHAPE, COLOR, ALPHA
"

ggplot(mpg) + 
    geom_point(aes(x = displ, y = hwy, color = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# impostare manualmente le aesthetics
ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy), color = "red")

# si può modificare anche la size in mm, e la shape

ggplot(mpg) + #sbagliata la collocazione di color! attenzione
    geom_point(mapping = aes(x = displ, y = hwy, color = "red")

# setting more aesthetics               
ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, color = cyl, shape = drv))

# impostare valori logici con il colore! ottima intuizione
ggplot(mpg) + 
    geom_point(aes(x = displ, y = hwy, color = displ < 5))

# aggiungere facet

# facet wrap, la variabile deve essere discreta
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_wrap(~ class, nrow = 2)

# possibile sezionare anche in combinazione tra due variabili
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(drv ~ class)

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(. ~ cyl)

# facet grid ma in orizzontale, Utile!
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(drv ~ .) # orizzontale

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    facet_grid(. ~ cyl) # verticale

"geometric objects ogni geometric object prende un 
argomento mapping
Non tutte le aesthetics vanno con tutti i geom, ad esempio
posso modificare la shape nei punti e la linetype nelle linee "


ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + # top graphics! separa le auto in tre linee sulla base di drv
    geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv), se = FALSE) +
    geom_point(mapping = aes(x = displ, y = hwy, color = drv))

# è possibile impostare una categoria gruppo invece che linetype

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

# si può anche togliere la legenda
ggplot(data = mpg) +
    geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)

# aggiungere geom
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

# introudce un doppione nel codice, meglio impostarlo subito

ggplot(data = mpg, aes(x = displ, y = hwy)) + # si applica ad ogni geom 
    geom_point(mapping = aes(color = class)) + # solo gli assi però
    geom_smooth(se = FALSE)

# possiamo scegliere dei filtri per ogni geom

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = class)) +
    geom_smooth(data = filter(mpg, class == "subcompact"),
                se = FALSE) # filtro per lo smooth

# altro esempio
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
    geom_point() + 
    geom_smooth(se = FALSE)

# esercizi
ggplot(mpg, aes(displ, hwy)) + 
    geom_point() + 
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy, group = drv)) + 
    geom_point()) + 
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy, color = drv)) + # color è applicato ad ogni geom
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy)) + # color solo ai point
    geom_point(aes(color = drv)) +
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy)) + # color solo ai point
    geom_point(aes(color = drv)) + # linetype ai geom
    geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(color = drv))


# trasformazioni statistiche

ggplot(data = diamonds) + 
    geom_bar(aes(x = cut)) # la statistica di base è count, ovvero la y

"
• Bar charts, histograms, and frequency polygons bin
    your data and then plot bin counts, the number of
    points that fall in each bin.
• Smoothers fit a model to your data and then plot 
    predictions from the model.
• Boxplots compute a robust summary of the 
    distribution and display a specially formatted 
    box.
"

# il valore di default per stat in geom_bar è count ovvero
# geom bar usa stat_count() posso farlo anche così
# ogni stat ha un geom di default e ogni geom ha uno stat di default!


ggplot(data = diamonds) +
    stat_count(mapping = aes(x = cut))

# posso modificare

demo <- tribble(
     ~a,     ~b,
    "bar_1", 20,
    "bar_2", 30,
    "bar_3", 40
)

ggplot(data = demo) +
    geom_bar(mapping = aes(x = a, y = b), stat = "identity")


# invece che count, potrebbe interessarci la frequenza

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
    stat_summary(mapping = aes(x = cut, y = depth),
                 fun.ymin = min,
                 fun.ymax = max,
                 fun.y = median)

# molto interessante
ggplot(diamonds, aes(price, ..density.., colour = cut)) +
    geom_freqpoly(binwidth = 500)

?geom_col
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
    geom_col()


# position adjustments and color
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = cut))

# se inseriamo un'altra variabile le barre si staccano da sole
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity)) # position adjustment di default

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity") 

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") 

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") 

# jitter aggiunge un pochino di rumore per ogni punto in modo che non si sovrappongano
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy),
               position = "jitter")

# il sistema di coordinate
# forse la cosa più difficile di ggplot
# default è coordinate cartesiane.

# coord_flip() cambia x e y
ggplot(data = mpg, aes(x = class, y = hwy)) +
    geom_boxplot()

ggplot(data = mpg, aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip()

# cord_quickmap seleziona l'aspetto corretto per le mappe
nz <- map_data("italy")
?map_data
ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", color = "black") +
    coord_quickmap()

# coord_polar utilizza le coordinate polari
# molto figo!!!


bar <- ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = cut),
             show.legend = FALSE,
             width = 1) +
    theme(aspect.ratio = 1) +
    labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


# molto figo, non facilissimo da interpretare
ggplot(data = mpg) +
    geom_bar(mapping = aes(x = drv, fill = drv), 
             position = "stack") +
    coord_polar()


?labs
#### R for Data Science Wickham ####

"
1. Importare i dati

2. Tidy data: pulirli e prepararli
ogni colonna una variabile
ogni riga un'osservazione

3. Trasformare i dati
creare nuove variabili di interesse

Tidy and Transform <- Wrangling

4. Visualization and Modeling

5. Communication

"

# Guida, Data Visualization ####

getwd()
setwd("/Users/fabiopaderi/Desktop/Data Science/R")
dir.create("R for Data Science")
setwd("./R for Data Science")

library(tidyverse)


mpg
# displ è la grandezza del motore in litri
# hwy è l'efficienza di consumo in autostrata

# geom_point -> Scatterplot

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))


"
è possibile modificare SIZE, SHAPE, COLOR, ALPHA
"

ggplot(mpg) + 
    geom_point(aes(x = displ, y = hwy, color = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# impostare manualmente le aesthetics
ggplot(mpg) +
    geom_point(mapping = aes(x = displ, y = hwy), color = "red")

# si può modificare anche la size in mm, e la shape

ggplot(mpg) + #sbagliata la collocazione di color! attenzione
    geom_point(mapping = aes(x = displ, y = hwy, color = "red")
               
               # setting more aesthetics               
               ggplot(mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy, color = cyl, shape = drv))
               
               # impostare valori logici con il colore! ottima intuizione
               ggplot(mpg) + 
                   geom_point(aes(x = displ, y = hwy, color = displ < 5))
               
               # aggiungere facet
               
               # facet wrap, la variabile deve essere discreta
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   facet_wrap(~ class, nrow = 2)
               
               # possibile sezionare anche in combinazione tra due variabili
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   facet_grid(drv ~ class)
               
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   facet_grid(. ~ cyl)
               
               # facet grid ma in orizzontale, Utile!
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   facet_grid(drv ~ .) # orizzontale
               
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   facet_grid(. ~ cyl) # verticale
               
               "geometric objects ogni geometric object prende un 
               argomento mapping
               Non tutte le aesthetics vanno con tutti i geom, ad esempio
               posso modificare la shape nei punti e la linetype nelle linee "
               
               
               ggplot(data = mpg) + 
                   geom_point(mapping = aes(x = displ, y = hwy))
               
               ggplot(data = mpg) + 
                   geom_smooth(mapping = aes(x = displ, y = hwy))
               
               ggplot(data = mpg) + # top graphics! separa le auto in tre linee sulla base di drv
                   geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv), se = FALSE) +
                   geom_point(mapping = aes(x = displ, y = hwy, color = drv))
               
               # è possibile impostare una categoria gruppo invece che linetype
               
               ggplot(data = mpg) +
                   geom_smooth(mapping = aes(x = displ, y = hwy))
               
               ggplot(data = mpg) +
                   geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
               
               ggplot(data = mpg) +
                   geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
               
               # si può anche togliere la legenda
               ggplot(data = mpg) +
                   geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)
               
               # aggiungere geom
               ggplot(data = mpg) +
                   geom_point(mapping = aes(x = displ, y = hwy)) +
                   geom_smooth(mapping = aes(x = displ, y = hwy))
               
               # introudce un doppione nel codice, meglio impostarlo subito
               
               ggplot(data = mpg, aes(x = displ, y = hwy)) + # si applica ad ogni geom 
                   geom_point(mapping = aes(color = class)) + # solo gli assi però
                   geom_smooth(se = FALSE)
               
               # possiamo scegliere dei filtri per ogni geom
               
               ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
                   geom_point(mapping = aes(color = class)) +
                   geom_smooth(data = filter(mpg, class == "subcompact"),
                               se = FALSE) # filtro per lo smooth
               
               # altro esempio
               ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
                   geom_point() + 
                   geom_smooth(se = FALSE)
               
               # esercizi
               ggplot(mpg, aes(displ, hwy)) + 
                   geom_point() + 
                   geom_smooth(se = FALSE)
               
               ggplot(mpg, aes(x = displ,y = hwy, group = drv)) + 
                   geom_point()) + 
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy, color = drv)) + # color è applicato ad ogni geom
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy)) + # color solo ai point
    geom_point(aes(color = drv)) +
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ,y = hwy)) + # color solo ai point
    geom_point(aes(color = drv)) + # linetype ai geom
    geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(color = drv))


# trasformazioni statistiche

ggplot(data = diamonds) + 
    geom_bar(aes(x = cut)) # la statistica di base è count, ovvero la y

"
• Bar charts, histograms, and frequency polygons bin
your data and then plot bin counts, the number of
points that fall in each bin.
• Smoothers fit a model to your data and then plot 
predictions from the model.
• Boxplots compute a robust summary of the 
distribution and display a specially formatted 
box.
"

# il valore di default per stat in geom_bar è count ovvero
# geom bar usa stat_count() posso farlo anche così
# ogni stat ha un geom di default e ogni geom ha uno stat di default!


ggplot(data = diamonds) +
    stat_count(mapping = aes(x = cut))

# posso modificare

demo <- tribble(
    ~a,     ~b,
    "bar_1", 20,
    "bar_2", 30,
    "bar_3", 40
)

ggplot(data = demo) +
    geom_bar(mapping = aes(x = a, y = b), stat = "identity")


# invece che count, potrebbe interessarci la frequenza

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
    stat_summary(mapping = aes(x = cut, y = depth),
                 fun.ymin = min,
                 fun.ymax = max,
                 fun.y = median)

# molto interessante
ggplot(diamonds, aes(price, ..density.., colour = cut)) +
    geom_freqpoly(binwidth = 500)

?geom_col
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
    geom_col()


# position adjustments and color
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = cut))

# se inseriamo un'altra variabile le barre si staccano da sole
ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity)) # position adjustment di default

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "identity") 

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") 

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") 

# jitter aggiunge un pochino di rumore per ogni punto in modo che non si sovrappongano
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy),
               position = "jitter")

# il sistema di coordinate
# forse la cosa più difficile di ggplot
# default è coordinate cartesiane.

# coord_flip() cambia x e y
ggplot(data = mpg, aes(x = class, y = hwy)) +
    geom_boxplot()

ggplot(data = mpg, aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip()

# cord_quickmap seleziona l'aspetto corretto per le mappe
nz <- map_data("italy")
?map_data
ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", color = "black")

ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", color = "black") +
    coord_quickmap()

# coord_polar utilizza le coordinate polari
# molto figo!!!


bar <- ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = cut),
             show.legend = FALSE,
             width = 1) +
    theme(aspect.ratio = 1) +
    labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


# molto figo, non facilissimo da interpretare
ggplot(data = mpg) +
    geom_bar(mapping = aes(x = drv, fill = drv), 
             position = "stack") +
    coord_polar()

# argomento labs
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p + labs(colour = "Cylinders")
p + labs(x = "New x label")
p + labs(title = "New plot title")
p + labs(title = "New plot title", subtitle = "A subtitle")
p + labs(caption = "(based on data from ...)")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point(position = "jitter") +
    geom_abline() +
    coord_fixed()


?geom_abline
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p + geom_vline(xintercept = 5)
p + geom_vline(xintercept = 1:5)
p + geom_hline(yintercept = 20)
p + geom_abline()
p + geom_abline(intercept = 20)

coef(lm(mpg ~ wt, data = mtcars)) # ha senso se si hanno i coefficienti
p + geom_abline(intercept = 37, slope = -5)

p + geom_smooth(method = "lm", se = FALSE) # con smooth lo fa direttamente

p <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point() +
    facet_wrap(~ cyl)

mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
p + geom_hline(aes(yintercept = wt), mean_wt)

ggplot(mtcars, aes(mpg, wt, colour = wt)) +
    geom_point() +
    geom_hline(aes(yintercept = wt, colour = wt), mean_wt) +
    facet_wrap(~ cyl)

"
LA GRAMMATICA DI GGPLOT
ggplot(data = <DATA>) +
      <GEOM_FUNCTION>(
mapping = aes(<MAPPINGS>),
stat = <STAT>,
position = <POSITION>) + 
<COORDINATE_FUNCTION> + 
<FACET_FUNCTION>

"














