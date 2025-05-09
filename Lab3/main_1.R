library(tidyverse)
library(arules)

# Wczytanie danych
groceries <- read.transactions("http://jolej.linuxpl.info/groceries.csv", sep = ",")
summary(groceries)

# Pare pierwszych transakcji
inspect(groceries[1:8], linebreak = FALSE)

# Ramka z czestotliwościami wystepowania danych produktow w transakcjach
groceries_frequency <-
  tibble(
    Items = names(itemFrequency(groceries)),
    Frequency = itemFrequency(groceries)
  )

summary(groceries_frequency)
head(groceries_frequency)

# 15 najczesciej kupowanych artykulów
groceries_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:15)

# Zasady
rules <- apriori(
  groceries,
  parameter = list(
    support = 0.01,
    confidence = 0.5,
    minlen = 2 # minimalna dlugosc reguly
  ))

inspect(rules)