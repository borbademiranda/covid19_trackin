library(tidyverse)
library(forecast)
library(tseries)
library(xts)

######################## DATA ####################
# downloading data
owid <- as.data.frame(data.table::fread("http://covid.ourworldindata.org/data/ecdc/total_cases.csv"))

# deaths
owid_deaths <- as.data.frame(data.table::fread('https://covid.ourworldindata.org/data/ecdc/total_deaths.csv'))

str(owid)

# as date
owid$date <- as.Date(owid$date)
owid_deaths$date <- as.Date(owid_deaths$date)

# adding time difference variable
owid$diff <- as.numeric(difftime(Sys.Date(), owid$date, units = 'days'))

summary(owid$date)


# transforming to time series date
owid_ts <- ts(owid$World)
owid_ts_deaths <- ts(owid_deaths$World)

# arima 021
a1 <- arima(owid_ts, order = c(0, 2, 1))

accuracy(a1)

# best arima
a2 <- auto.arima(owid_ts)
accuracy(a2)

# deaths
a2_deaths <- auto.arima(owid_ts_deaths)

# forecast
fc <- forecast(a2, h = 7)

fc_deaths <- forecast(a2_deaths, h = 7)

# extracting and binding dataframes
bd_ar <- data.frame(cases = a2$x, 
                    data = owid$date, 
                    Valor = 'Observado',
                    lower = NA,
                    upper = NA) %>%
  bind_rows(data.frame(cases = fc$mean, 
                   data = seq.Date(from = Sys.Date() + 1, length.out = 7, by = 'day'), 
                   Valor = 'Predito',
                   lower = fc$lower,
                   upper = fc$upper))

bd_ar_deaths <- data.frame(cases = a2_deaths$x, 
                           data = owid_deaths$date, 
                           Valor = 'Observado',
                           lower = NA,
                           upper = NA) %>%
  bind_rows(data.frame(cases = fc_deaths$mean, 
                       data = seq.Date(from = Sys.Date() + 1, length.out = 7, by = 'day'), 
                       Valor = 'Predito',
                       lower = fc_deaths$lower,
                       upper = fc_deaths$upper))

# plot
l1 <- ggplot(data = bd_ar, aes(x = data, y = cases, color = Valor)) +
  geom_line() +
  theme_classic() +
  labs(x = '', y = '') +
  theme(legend.position = 'bottom') +
  geom_hline(yintercept = 2000000, linetype = 2, color = 'red') +
  geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), colour = 'grey70', alpha = .3)
ggsave('./data/arima1.png', plot = l1, device = 'png', width = 7, height = 4)

l1_d <- ggplot(data = bd_ar_deaths, aes(x = data, y = cases, color = Valor)) +
  geom_line() +
  theme_classic() +
  labs(x = '', y = '') +
  theme(legend.position = 'bottom') +
  #geom_hline(yintercept = 2000000, linetype = 2, color = 'red') +
  geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), colour = 'grey70', alpha = .3)
ggsave('./data/arima1_deaths.png', plot = l1_d, device = 'png', width = 7, height = 4)

# setting na to zero
owid[is.na(owid)] <- 0

owid_deaths[is.na(owid_deaths)] <- 0

# subsetting brazil
owid$Brazil <- as.integer(owid$Brazil)

owid_deaths$Brazil <- as.integer(owid_deaths$Brazil)

# filter Brazil
ts_br <- ts(owid$Brazil)

ts_br_deaths <- ts(owid_deaths$Brazil)

# arima Brazil
a3 <- auto.arima(ts_br)
accuracy(a3)

a3_deaths <- auto.arima(ts_br_deaths)
accuracy(a3_deaths)

# forecast brazil
fc2 <- forecast(a3, h = 7)

fc2_deaths <- forecast(a3_deaths, h = 7)

# data frame
bd_ar_br_deaths <- data.frame(cases = a3_deaths$x, 
                       data = owid_deaths$date, 
                       Valor = 'Observado',
                       lower = NA,
                       upper = NA) %>%
  bind_rows(data.frame(cases = fc2_deaths$mean, 
                       data =  seq.Date(from = Sys.Date() + 1, length.out = 7, by = 'day'), 
                       Valor = 'Predito',
                       lower = fc2_deaths$lower,
                       upper = fc2_deaths$upper))


l2 <- ggplot(data = bd_ar_br_deaths, aes(x = data, y = cases, color = Valor)) +
  geom_line(group = 1) +
  theme_classic() +
  labs(x = '', y = '') +
  theme(legend.position = 'bottom') +
  geom_hline(yintercept = 30000, linetype = 2, color = 'red') +
  geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), colour = 'grey70', alpha = .3) +
  theme(axis.text.x = element_text(angle = 90))
ggsave('./data/arima_br.png', plot = l2, device = 'png', width = 7, height = 4)

l2_d <- ggplot(data = bd_ar_br_deaths, aes(x = data, y = cases, color = Valor)) +
  geom_line(group = 1) +
  theme_classic() +
  labs(x = '', y = '') +
  theme(legend.position = 'bottom') +
  #geom_hline(yintercept = 30000, linetype = 2, color = 'red') +
  geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), colour = 'grey70', alpha = .3) +
  theme(axis.text.x = element_text(angle = 90))
ggsave('./data/arima_br_deaths.png', plot = l2_d, device = 'png', width = 7, height = 4)


# forecasting next seven days
options(scipen = 999)

plot1 <- autoplot(fc) +
 theme_classic() +
  labs(x = '', y = '') +
  ggtitle('') +
  geom_hline(yintercept = 2000000, linetype = 2) +
  scale_fill_manual(values = c('Observado', 'Previsão'))

ggsave('./data/arima.png', plot = plot1, device = 'png', width = 6, height = 4)

############################ DEATHS ##########################


