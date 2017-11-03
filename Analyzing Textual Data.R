#### Text as Data ####

"""
esistono diversi campi di ricerca che si occupano dell'analisi di dati di 
testo. come ad esempio il natural language processing, o 
la linguistica computazionale. Si lavora con documenti di testo
e si estrae significato da essi.
"""

"""
Lavorare con il testo richiede strumenti nuovi.



Regular expression using Machbet
useremo il textmining per esplorare la tragedia Macbeth
il testo è già presente nel pacchetto mdsr
"""

library(mdsr)
# oppure
macbeth_url <- "http://www.gutenberg.org/cache/epub/1129/pg1129.txt"
Macbeth_raw <- RCurl::getURL(macbeth_url)

data("Macbeth_raw")

"""
notare che l'oggetto macbeth raw è composto da una singola 
stringa di testo. è un character di lunghezza 1.
Per lavorare con esso, vogliamo slpittare questa
singola stringa in un vettore di stringhe tramite la funzione
strsplit(). per fare ciò dobbiamo specificare la fine della linea dei 
caratteri che in questo caso sono \r\n
"""

# strsplit riporta una lista: vogliamo solo il primo elemento
macbeth <- strsplit(Macbeth_raw, "\r\n")[[1]]

macbeth[300:310] 


"""
ogni linea di dialogo inizia con due spazi e il nome dell'
attore in lettere maiuscole. La potenza del text mining 
deriva dal quantificare le idee che sono incorporate nel testo
Per esempio, quante volte parla Macbeth nella tragedia??

Se avessimo una copia fisica, come faremmo a contarle??
dovremmo scorrere tutte le pagine e contare? è scalabile questa
soluzione? posso farlo per tutte le commedie di shakespeare
o per tutte le commedie inglesi?? No, posso contare i pattern
nel testo.

La funzione grep cerca il pattern all'interno di un vettore
x
Il primo argomento è il pattern, o la regular expression
mentre il secondo argomento deve essere un vettore di 
caratteri in cui vogliamo trovare il pattern. mettendo
value = TRUE, la funzione grep riporta gli indici in ordine
dei pattern che trova
"""
length(macbeth)
class(macbeth)

macbeth_lines <- grep("  MACBETH", macbeth, value = TRUE)

macbeth_lines[1:10]

length(macbeth_lines) #147 volte parla Macbeth.


length(grep("  MACDUFF", macbeth)) # 60 volte parla Macduff


"""
la funzione grepl() usa la medesima sintassi di grep ma riporta un
logical vector della lunghezza delle vettore character. 
Quindi, mentre la lunghezza del vettore grep() è il numero
di matches corretti, la lunghezza di grepl() è la stessa del
vettore originale.
"""

length(grep("  MACBETH", macbeth))

length(grepl("  MACBETH", macbeth))

"""
per estrare il pezzo di ogni matching line si può usare
la funzione str_extract() dal pacchetto stringr
"""

library(stringr)

pattern <- "  MACBETH"

grep(pattern, macbeth, value = TRUE) %>%
  str_extract(pattern) %>%
  head()

"""
la sintassi delle espressioni regolarei può essere molto 
potente e quindi diventare molto complicata.
é complicata da imparare e ci vuole tempo
"""


"""
METACARATTERI: il punto "." è un metacarattere che matche
tutti i caratteri.
"""


head(grep("MAC.", macbeth, value = TRUE)) #tutto quello che inizia con MAC

head(grep("MACBETH\\.", macbeth, value = TRUE))


"""
SETS DI CARATTERI. Usare le parentesi quadre per definire una
serie di caratteri da matchare. Questa sintassi riporta tutte
le linee che contengono MAC seguito da qualunque lettera maiuscola
diversa da A
"""

head(grep("MAC[B-Z]", macbeth, value = TRUE))

"""
ALTERNAZIONI, per cercare tra specifiche alternative si usa |
tra parentesi, questo trova ogni pattern che meccia MACB o MACD
"""

head(grep("MAC(B|D)", macbeth, value = TRUE))


"""
ANCORAGGI, usare ^ per ancorare un pattern all'inizio di un pezzo
di testo, e $ per ancorarlo alla fine
"""


head(grep("^  MAC[B-Z]", macbeth, value = TRUE))

"""
RIPETIZIONI possiamo anche specificare il numero di ripetizioni
che vogliamo occorrano, ? indica zero o una volta, * indica zero o più volte
e + indica una o pià volte. Questa quantificazione è applicata
all'elemento precedente nel pattern, ovvero lo spazio
"""

head(grep("^ ?MAC[B-Z]", macbeth, value = TRUE))

head(grep("^ *MAC[B-Z]", macbeth, value = TRUE))

head(grep("^ +MAC[B-Z]", macbeth, value = TRUE))


"""
esempio. come possiamo analizzare le parole in macbeth?
Quante volte parlano gli attori???
"""


Macbeth <- grepl(" MACBETH\\.", macbeth) 
LadyMacbeth <- grepl(" LADY MACBETH\\.", macbeth) 
Banquo <- grepl(" BANQUO\\.", macbeth)
Duncan <- grepl(" DUNCAN\\.", macbeth)

"""
Per plottarlo però dobbiamo convertire questi vettori
logici in numerici, e tidy up the data.
"""

library(tidyr)

speaker_freq <- data.frame(Macbeth, LadyMacbeth, Banquo, Duncan) %>%
  mutate(line = 1:length(macbeth)) %>%
  gather(key = "character", value = "speak", -line) %>%
  mutate(speak = as.numeric(speak)) %>%
  filter(line > 218 & line < 3182)

glimpse(speaker_freq)

acts_idx <- grep("^ACT [I|V]+", macbeth)
acts_labels <- str_extract(macbeth[acts_idx], "^ACT [I|V]+")
acts <- data.frame(line = acts_idx, labels = acts_labels)

ggplot(data = speaker_freq, aes(x = line, y = speak)) +
  geom_smooth(aes(color = character), method = "loess", se = 0, span = 0.4, size = 0.5) +
  geom_vline(xintercept = acts_idx, color = "darkgray", lty = 3) +
  geom_text(data = acts, aes(y = 0.085, label = labels),
            hjust = "left", color = "darkgray") +
  ylim(c(0, NA)) + xlab("Line Number") + ylab("Proportion of Speeches")


#### ANALYZING TEXUTAL DATA ####
"""
Analyzing textual data.

l'archivio arXiv è una repository di paper scientifici prima che 
vengano pubblicati. il pacchetto di r, aRxiv fornisce un API
per estrarre questi dati dall'archivio. arxiv.org.
ora esploreremo 95 papers che matchano il termine data science
nella repository
"""

library(aRxiv)

Papers <- arxiv_search(query = '"Stereotype"', limit = 200)

head(Papers)

"""
Ci sono due colonne nel data frame, submitted e updated che sono
le date storiche, ma sono stored come character e non come date
"""

library(lubridate)

Papers <- Papers %>%
  mutate(submitted = ymd_hms(submitted), updated = ymd_hms(updated))

glimpse(Papers)

tally(~year(submitted), data = Papers)

"""
Il Text mining è spesso fatto non solo su un documento ma su 
una serie di documenti chiamato corupus. bisogna creare un text
corpus per estrarre le informazioni.
"""

library(tm)

Corpus = with(Papers, VCorpus(VectorSource(abstract)))

Corpus[[1]] %>%
  as.character() %>%
  strwrap()

"""
Le parole inutili adesso vengono eliminate, quelle che in inglese
si chiamano stop words.
"""

Corpus <- Corpus %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english"))
strwrap(as.character(Corpus[[1]]))

"""
Word Clouds

adesso abbiamo trasformato qualcosa che era leggibile in DATI.
"""


library(wordcloud)

wordcloud(Corpus, max.words = 100, scale = c(8,1),
          colors = topo.colors(n = 30), random.color = TRUE)

"""
Document Term Matrix

un'altra importante tecnica di text mining coinvolge il calcolo 
term document matrix. semplicemente è una misura della prevalenza all'interno di
un testo o documenti di una certa parola.
"""


DTM <- DocumentTermMatrix(Corpus, control = list(weighting = weightTfIdf))
DTM

findFreqTerms(DTM, lowfreq = 0.4)

DTM %>% as.matrix() %>%
  apply(MARGIN = 2, sum) %>%
  sort(decreasing = TRUE) %>%
  head(9)


"""
In questo modo, molto importante, andiamo 
ad identificare quali parole si associano maggiormente
con il termine gender!
"""

findAssocs(DTM, terms = "gender", corlimit = 0.5)



"""
Ingesting TEXT 

I dati si possono ottenere anche da internet, quindi perchè non 
analizzare il testo di internet??
"""


library(rvest)
library(tidyr)
library(methods)

url <- "http://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Beatles"

tables <- url %>%
  read_html() %>%
  html_nodes(css = "table")
song <- html_table(tables[[5]])

glimpse(song)

# bisogna pulire un po i dati

song <- song %>%
  mutate(Title = gsub('\\"', "", title), Year = as.numeric(Year)) %>%
  rename(songwriter = `Songwriter(s)`)

tally(~songwriter, data = song) %>%
  sort(decreasing = TRUE) %>%
  head()

length(grep("McCartney", song$songwriter))
length(grep("Lennon", song$songwriter))


song %>%
  filter(grepl("(McCartney|Lennon) .*(McCartney|Lennon)", songwriter)) %>%
  select(Title) %>%
  head()


song_titles <- VCorpus(VectorSource(song$Title)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  DocumentTermMatrix(control = list(weighting = weightTfIdf))
findFreqTerms(song_titles, 25)

# quali sono le parole più usate! love you

"""
Scraping Data From Twitter
"""

library(twitteR)

setup_twitter_oauth(consumer_key = "clW9xtLHgUG6KNCSd5EoC2vD9",
                    consumer_secret = "LN5PUTBszLri7We6y1WIOJc4ySTfkJ7XSRlAUOmKMgtZXifkIh",
                    access_token = "908064517959057411-5v4NrF2YCwwyzFgLXcJ9JDQoYanc5Xq",
                    access_secret = "5Po0B5XUrMX2L5lbxyTkUAaWUzOocUvRocEAqxjzAUmOI")

tweets <- searchTwitter("#aging", lang = "en", n = 1000,
                        retryOnRateLimit = 100)

class(tweets)
class(tweets[[1]]) # JSON objects ma è possibile collassarli in data frame

tweet_df <- twListToDF(tweets) %>% as.tbl()

tweet_df %>%
  select(text) %>%
  head()

ggplot(data = tweet_df, aes(x = nchar(text))) +
  geom_density(size = 2) +
  geom_vline(xintercept = 140) +
  scale_x_continuous("Number of Characters")

ggplot(data = tweet_df, aes(x = retweetCount)) +
  geom_density(size = 2)

"""
Siccome tweetter non permette di collezionare più di un tot di dati
è possibile creare un database di tweet in modo da collezionarne
più possibile.
Costruire un database di tweet

"""

tweet_db <- tempfile()
register_sqlite_backend(tweet_db)
store_tweets_db(tweets)

tweets_src <- src_sqlite(tweet_db)
old_tweets <- tweets_src %>% tbl("tweets")
glimpse(old_tweets)

interesting_tweets <- old_tweets %>%
  collect() %>%
  filter(grepl("#sometext", text))
nrow(interesting_tweets) / nrow(collect(old_tweets))


"""
Trends

Twitter tiene conto degli hashtag più influenti e questi sono
tracciabili, ovvero è possibile vedere come certe popolazioni rispondono a 
a news e world events.

"""


library(ggmap)

trend = geocode("Palazzo Nuovo, Torino")

with(trend, closestTrendLocations(lat = lat, long = lon))

head(getTrends(2458410), 100)






