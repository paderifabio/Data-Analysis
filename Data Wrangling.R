#### DATA WRANGLING ####

#select() take a subset of the columns (i.e., features, variables) 
#filter() take a subset of the rows (i.e., observations) 
#mutate() add or modify existing columns
#arrange() sort the rows
#summarize() aggregate the data across rows (e.g., group it according to some criteria)
# ognuna di queste funzioni restituisce un data frame
library(dplyr)
library(mdsr)
 
#### Select and Filter ####

data("presidential")

filter(presidential, party == "Republican")
select(presidential, name,party) # non importa mettere le virgolette

select(filter(presidential, 
              start > 1973 & party == "Democratic"), name)

# filter is nested in select però quando sono tante le operazioni diventa complicato capire
# meglio usare l'operatore pipe perchè più facile da leggere --> %>%

presidential %>% 
  filter(start > 1973 & party == "Democratic") %>%  #stessa cosa del comando precedente
  select(name)


# dataframe %>% filter(condition)
# is equivalent to filter(dataframe, condition)


#### Mutate, Rename, Order, Summarize ####

library(lubridate) # compute the number of exact years (eyears(1)())
#that elapsed since during the interval() 
#from the start until the end of each president’s term

mypresidents <- presidential %>%
  mutate(term.length = interval(start,end) / dyears(1))
mypresidents

mypresidents <- mypresidents %>% mutate(elected = year(start) - 1)
mypresidents

mypresidents <- mypresidents %>%
  mutate(elected = ifelse((elected %in% c(1962, 1973)), NA, elected))
mypresidents

mypresidents <- mypresidents %>% rename(term_length = term.length)
mypresidents

# la funzione sort ordina un vettore, per ordinare un data frame bisogna usare arrange()

mypresidents %>% arrange(desc(term_length))

mypresidents %>% arrange(desc(term_length), party, elected)

mypresidents %>% #Note that every variable in the output is defined by operations performed on vectors—not on individual values
  summarize(
    N = n(), first_year = min(year(start)), last_year = max(year(end)),
    num_dems = sum(party == "Democratic"),
    years = sum(term_length),
    avg_term_length = mean(term_length)
  )

mypresidents %>%
  group_by(party) %>%
  summarize(
    N = n(), first_year = min(year(start)), last_year = max(year(end)),
    num_dems = sum(party == "Democratic"),
    years = sum(term_length),
    avg_term_length = mean(term_length)
  )

# dopo summarize conviene sempre usare N = n()

#### Extended Example ####

library(Lahman)
dim(Teams)


myMets = Teams %>%
  select(yearID, teamID, W, L) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012)

myMets = Teams %>%
  filter(teamID == "NYM" & yearID %in% 2004:2012) %>%
  select(teamID, yearID, W,L)

myMets %>%
  mutate(dif_score = W - L) %>%
  arrange(desc(dif_score)) %>%
  mutate(good_season = ifelse(dif_score > 0, 1,0))

myMets %>%
  mutate(perc_win = (W/(W+L))*100) %>%
  arrange(desc(perc_win))

metsBen <- Teams %>% 
  select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012)
metsBen

metsBen = metsBen %>%
  rename(RS = R) # ricorda new name = old name
metsBen

metsBen %>%
  mutate(punteggio = 1/(1 + (RA/RS)^2)) %>%
  mutate(perc_win = (W/(W+L))*100) %>%
  arrange(desc(perc_win))

favstats(~W, data = metsBen)

metsBen %>%
  summarise(
    numero_anni = n(), total_W = sum(W), total_L = sum(L),
    total_WPct = sum(W) / sum(W + L)
  )

metsBen = metsBen %>%
  mutate(
    gm = ifelse(yearID == 2004, "Duquette", ifelse(yearID >=2011, "Alderson", "Minaya"))
  )

metsBen %>%
  group_by(gm) %>%
  summarise(
    num_years = n(), total_W = sum(W), total_L = sum(L),
    total_WPct = sum(W) / sum(W + L) 
      )

Teams %>%
  select(yearID, teamID, W, L, R, RA) %>%
  filter(teamID == "NYN" & yearID %in% 2004:2012) %>%
  rename(RS = R) %>%
  mutate(
    WPct = W / (W + L), WPct_hat = 1 / (1 + (RA/RS)^2),
    W_hat = WPct_hat * (W + L),
    gm = ifelse(yearID == 2004, "Duquette",
                ifelse(yearID >= 2011, "Alderson", "Minaya"))) %>%
  group_by(gm) %>%
  summarize(
    num_years = n(), total_W = sum(W), total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), sum_resid = sum(W - W_hat)) %>%
  arrange(desc(sum_resid))

Teams %>% select(yearID, teamID, franchID, W, L, R, RA) %>%
  filter(yearID %in% 2004:2012) %>%
  rename(RS = R) %>%
  mutate(
    WPct = W / (W + L), WPctHat = 1 / (1 + (RA/RS)^2),
    WHat = WPctHat * (W+L)) %>%
  group_by(franchID) %>%
  summarize(
    numYears = n(), totalW = sum(W), totalL = sum(L),
    totalWPc = sum(W) / sum(W + L), sumResid = sum(W - WHat)) %>%
  arrange(sumResid) %>%
  print(n = 6)

#### Combine multiple tables ####

library(nycflights13)
head(flights)  
head(airlines)

flightsjoined = flights %>%
  inner_join(airlines, by = c("carrier" = "carrier"))
glimpse(flightsjoined)

# nuova variabile di nome "name"

flightsjoined %>%
  select(carrier, name, flight, origin, dest) %>%
  head(3)

#### Extended Example ####

# Manny Ramirez # 

manny = filter(Batting, playerID == "ramirma02")
nrow(manny)

manny %>% summarize (
  span = paste(min(yearID), max(yearID), sep = "-"),
  numYears = n_distinct(yearID), numTeams = n_distinct(teamID),
  BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)
)

# ha giocato allo stesso modo nelle sue cinque squadre?

manny %>% group_by(teamID) %>%
  summarize (
  span = paste(min(yearID), max(yearID), sep = "-"),
  numYears = n_distinct(yearID), numTeams = n_distinct(teamID),
  BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)
) %>% arrange(span)


manny %>% group_by(lgID) %>%
  summarize (
    span = paste(min(yearID), max(yearID), sep = "-"),
    numYears = n_distinct(yearID), numTeams = n_distinct(teamID),
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)
  ) %>% arrange(span)

manny %>% 
  group_by(yearID) %>%
  summarize(tHR = sum(HR)) %>%
  filter(tHR >=30) %>%
  nrow()

# Exercises

head(flights)
names(flights)
aggregate(dep_delay~month, FUN = sum, data = flights)

flights %>%
  select(year, tailnum, hour, origin) %>%
  filter(year == 2013 & origin == "JFK") %>%
  group_by(tailnum) %>%
  summarise(
    n = n(), ore = sum(hour)
  ) %>%
  arrange(desc(ore))

planesjoint = flights %>%
  inner_join(planes, by = ("tailnum" = "tailnum"))
glimpse(planesjoint)

planesjoint %>%
  select(year.y, tailnum, origin, year.x) %>%
  filter(year.x == 2013 & origin == "JFK") %>%
  arrange(year.y) %>% 
  summarise(
    n = n(), n_planes = n_distinct(tailnum)
  )

summary(planesjoint)





