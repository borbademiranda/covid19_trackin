# packages
library(twitteR)
library(rtweet)
library(tidyverse)
library(stringr)
library(syuzhet)
library(bigmemory)
library(tidytext)

options(scipen = 999)

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

# loading dataset
load('./data/twitter.RData')

# downloading new tweets
tw$status_id <- as.character(tw$status_id)

tw$status_id <- order(tw$status_id)

row

tw_b_n <- search_tweets('bolsonaro OR presidente', 
                        include_rts = F, n = 10000000, retryonratelimit = T, 
                        lang = 'pt', since = since_id('1247879535703601152'))
tw_b_n$subject <- 'Bolsonaro'

save(tw, file = 'data/twitter_bols.RData')

summary(tw_b_n$created_at)

tw_m_n <- search_tweets('mandetta OR "ministro da saúde"', 
                        include_rts = F, n = 10000000, retryonratelimit = T, 
                        lang = 'pt', since = str_sub(summary(tw$created_at)[[6]], end = -10))
tw_m_n$subject <- 'Mandetta'

tw_n <- rbind(tw_b_n, tw_m_n)

save(tw_n, file = './data/twitter_n.RDAta')

############### LOADING DATA ################
load('./data/twitter_n.RDAta')

# subsetting data from last database date
tw_n <- tw_n[tw_n$status_id != tw$status_id,]

# binding rows
tw <- bind_rows(tw, tw_n)

# returning unique rows
tw <- tw %>% distinct(status_id, subject, .keep_all = T)

names(tw)

tw <- tw[, -c(92:101)]

# getting sentiments and binding data
tw <- cbind(tw, get_nrc_sentiment(tw$text, language = 'portuguese'))

# saving dataset
save(tw, file = './data/twitter2.RData')

##################### LOADING DATA ##################
load('./data/twitter2.RData')

names(tw)

# subsetting columns
columns <- c('status_id', 'user_id', 'created_at', 'text', 'hashtags', 'symbols',
             'lang', 'country', 'geo_coords', 'name', 'location', 'account_created_at',
             'subject', 'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness',
             'surprise', 'trust', 'negative', 'positive')

tw <- tw[columns]

gc()

# search new tweets
new <- search_tweets('bolsonaro OR presidente', 
                     include_rts = F, n = 10000000, retryonratelimit = T, 
                     lang = 'pt', since = str_sub(summary(tw$created_at)[[6]], 
                                                  end = -10)) %>%
  mutate(subject = 'Bolsonaro') %>%
  rbind(search_tweets('mandetta OR "ministro da saúde"', 
                include_rts = F, n = 10000000, retryonratelimit = T, 
                lang = 'pt', since = str_sub(summary(tw$created_at)[[6]], 
                                             end = -10)) %>%
        mutate(subject = 'Mandetta'))

save(new, './data/new_tw.RData')
system('shutdown -s')

# getting sentiments
new <- cbind(new[columns], get_nrc_sentiment(new$text, language = 'portuguese'))
  
# binding with data.frame
tw <- rbind(tw, new)

tw <- tw[, -c(92:101)]

# tokenizing tweets
rm(tw_n)

# restarting session
.rs.restartR()

load('./data/tokens.RData')

summary(tok$created_at)

# tokenizing text
tok <- tw %>%
  filter(created_at < mean(tw$created_at)) %>%
  unnest_tokens(term, text) %>%
  select(status_id, term, subject, created_at) %>%
  merge(get_sentiment_dictionary('nrc', language = 'portuguese'), 
        by.x = 'term', by.y = 'word')

quantile(tw$created_at, .75)

tok <- tok %>% 
  rbind(tw %>%
          filter(created_at >= mean(tw$created_at) & 
                   created_at <= quantile(tw$created_at, .75)) %>%
          unnest_tokens(term, text) %>%
          select(status_id, term, subject, created_at) %>%
          merge(get_sentiment_dictionary('nrc', language = 'portuguese'), 
                by.x = 'term', by.y = 'word'))

tok <- tok %>% 
  rbind(tw %>%
          filter(created_at > quantile(tw$created_at, .75)) %>%
          unnest_tokens(term, text) %>%
          select(status_id, term, subject, created_at) %>%
          merge(get_sentiment_dictionary('nrc', language = 'portuguese'), 
                by.x = 'term', by.y = 'word'))

# binding with last token object
tok <- rbind(tok, tok2)

# saving token object
save(tok, file = './data/tokens.RData')

# removing objects
rm(list = ls())
