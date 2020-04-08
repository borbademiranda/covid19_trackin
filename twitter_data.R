# packages
library(twitteR)
library(rtweet)
library(tidyverse)
library(stringr)
library(syuzhet)

# IF YOU ALREAD DONWLOADED TWITTER DATA, PROCEED TO THE NEXT SECTION ########


# downloading tweets
#tw <- search_tweets('bolsonaro OR presidente OR ministro da saude OR mandetta', 
 #                   include_rts = F, n = 10000000, retryonratelimit = T, 
  #                  lang = 'pt', type = 'recent')

# transformando em data frame
#tw2 <- as.data.frame(tw)

# searching tweets about bolsonaro
tw_b <- search_tweets('bolsonaro OR presidente', 
                      include_rts = F, n = 10000000, retryonratelimit = T, 
                      lang = 'pt')

# adding subject variable
tw_b$subject <- 'Bolsonaro'

# searching tweeets about mandetta
tw_m <- search_tweets('mandetta OR "ministro da saúde"', 
                      include_rts = F, n = 10000000, retryonratelimit = T, 
                      lang = 'pt')

tw_m$subject <- 'Mandetta'

tw <- rbind(tw_b, tw_m)

save(tw, file = './data/twitter2.RData')

####### DOWNLOADING TWEETS OF THE CURRENT DATE ########

# loading existing dataset
load('./data/twitter2.RData')

# downloading new tweets
search_tweets('bolsonaro OR presidente OR ministro da saude OR mandetta', 
              include_rts = F, 
              n = 10000000, 
              retryonratelimit = T, 
              lang = 'pt', 
              since = str_sub(summary(tw3$created_at)[[6]], end = -10)) %>%
  bind_rows(tw3) -> tw3

table(tw3$status_id)

# getting sentiments
tw <- cbind(tw, get_nrc_sentiment(tw$text, language = 'portuguese'))
  
# salvando dados do dia
save(tw, file ='./data/twitter.RData')

rm(list = ls())
