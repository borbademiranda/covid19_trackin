library(tidyverse)
library(tidytext)
library(stringr)
library(rvest)
library(xml2)
library(tm)
library(data.table)

##### SENTIMENT DICTIONARY #####

# negative adjetives
an <- as.data.frame(
  fread('https://www.linguateca.pt/Repositorio/ReLi/ADJ_Negativos.txt', 
        header = F, strip.white = F, stringsAsFactors = F, sep = '[', fill = T))
an <- an[,2] %>% as.data.frame()
names(an) <- 'V2'

# negative expressions
exn <-  fread('https://www.linguateca.pt/Repositorio/ReLi/MWE_Negativos.txt', 
        header = F, strip.white = F, stringsAsFactors = F, fill = T, sep = '[')

exn <- exn[, 2] %>% as.data.frame()

# negative substantives
subn <- fread('https://www.linguateca.pt/Repositorio/ReLi/MWE_Negativos.txt', 
              header = F, strip.white = F, stringsAsFactors = F, fill = T, 
              sep = '[')
subn <- subn[, 2]

# verbos negativos
vn <- fread('https://www.linguateca.pt/Repositorio/ReLi/Verbos_Negativos.txt',
            header = F, strip.white = F, stringsAsFactors = F, fill = T, 
            sep = '[')
vn <- vn[, 2]

# positive adjectives
ap <- fread('https://www.linguateca.pt/Repositorio/ReLi/ADJ_Positivos.txt',
            header = F, strip.white = F, stringsAsFactors = F, fill = T, 
            sep = '[')
ap <- ap[,2]

# positive expressions
exp <- fread('https://www.linguateca.pt/Repositorio/ReLi/MWE_Positivos.txt', 
             header = F, strip.white = F, stringsAsFactors = F, fill = T, 
             sep = '[')
exp <- exp[,2]

# positive verbs
vp <- fread('https://www.linguateca.pt/Repositorio/ReLi/Verbos_Positivos.txt', 
            header = F, strip.white = F, stringsAsFactors = F, fill = T, 
            sep = '[')
vp <- vp[, 2]

# positive substantives
sp <- fread('https://www.linguateca.pt/Repositorio/ReLi/Subst_Positivos.txt',
            header = F, strip.white = F, stringsAsFactors = F, fill = T, 
            sep = '[')  
sp <- sp[, 2]

# criando data frame com as polaridades
dfPolaridades <- an %>% 
  mutate(word = V2, polaridade = -1, tipo='adjetivo', sentimento='negativo') %>%
  select(word,polaridade,tipo,sentimento) %>%
  arrange(word)

dfPolaridades <- bind_rows(dfPolaridades, exn %>%
                             mutate(word = V2, polaridade = -1, tipo='expressão', sentimento='negativo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))
dfPolaridades <- bind_rows(dfPolaridades, subn %>%
                             mutate(word = V2, polaridade = -1, tipo='substantivo', sentimento='negativo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))
dfPolaridades <- bind_rows(dfPolaridades, vn %>%
                             mutate(word = V2, polaridade = -1, tipo='verbo', sentimento='negativo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))

# positivos
dfPolaridades <- bind_rows(dfPolaridades, ap %>%
                             mutate(word = V2, polaridade = 1, tipo='adjetivo', sentimento='positivo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))
dfPolaridades <- bind_rows(dfPolaridades, exp %>%
                             mutate(word = V2, polaridade = 1, tipo='expressão', sentimento='positivo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))
dfPolaridades <- bind_rows(dfPolaridades, sp %>%
                             mutate(word = V2, polaridade = 1, tipo='substantivo', sentimento='positivo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))
dfPolaridades <- bind_rows(dfPolaridades, vp %>%
                             mutate(word = V2, polaridade = 1, tipo='verbo', sentimento='positivo') %>%
                             select(word,polaridade,tipo,sentimento) %>%
                             arrange(word))

table(dfPolaridades$sentimento)

# loading kaggle dataset
poskaggle <- read.csv("./data/sentimentos/positive_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")

negkaggle <- read.csv("./data/sentimentos/negative_words_pt.txt", header = F, sep = "\t", strip.white = F, 
                      stringsAsFactors = F, encoding="UTF-8")


# kaggle data modifying names
names(negkaggle) <- 'word'
names(poskaggle) <- 'word'

# bind with data frame
dfPolaridades <- bind_rows(dfPolaridades, negkaggle %>%
                           mutate(polaridade = -1))
dfPolaridades <- bind_rows(dfPolaridades, poskaggle %>%
                             mutate(polaridade = 1))

# removendo termos repetidos
dfPolaridades_unique <- dfPolaridades[!duplicated(dfPolaridades$word),]


# excluding ] from rows
dfPolaridades_unique$word <- gsub(']', '', dfPolaridades_unique$word)

# saving dataset
write.csv(dfPolaridades, './data/sentimentos/dictionary_pt.csv')

rm(list = ls())

# tokenizing text




