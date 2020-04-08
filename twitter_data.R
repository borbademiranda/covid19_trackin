# packages
library(twitteR)
library(rtweet)
library(tidyverse)
library(stringr)

# IF YOU ALREAD DONWLOADED TWITTER DATA, PROCEED TO THE NEXT SECTION ########


# downloading tweets
tw <- search_tweets('bolsonaro OR presidente OR ministro da saude OR mandetta', 
                    include_rts = F, n = 10000000, retryonratelimit = T, 
                    lang = 'pt', type = 'recent')

# transformando em data frame
tw2 <- as.data.frame(tw)

####### DOWNLOADING TWEETS OF THE CURRENT DATE ########

# loading existing dataset
load('./data/twitter.RData')

# downloading new tweets
search_tweets('bolsonaro OR presidente OR ministro da saude OR mandetta', 
              include_rts = F, 
              n = 10000000, 
              retryonratelimit = T, 
              lang = 'pt', 
              since = str_sub(summary(tw3$created_at)[[6]], end = -10)) %>%
  bind_rows(tw3) -> tw3

# getting sentiments
get_nrc_sentiment(tw3$text, language = 'portuguese') %>%
  cbind(tw3) -> tw3

# salvando dados do dia
save(tw3, file ='./data/twitter.RData')
