library('tidyverse')
library('car')
library('RcmdrMisc')
library('sandwich')
library('relimp')
library('corrplot')
library("caTools")
library('caret')
library('ipred')
library('e1071')

Sys.setlocale("LC_ALL", "Polish_Poland.UTF-8")

library(nnet)
library(caret)
library(NeuralNetTools)

# Wczytaj dane
bank <- read.csv("Lab4/data/bank-full.csv", sep = ";", stringsAsFactors = TRUE)

# Sprawdź dane
str(bank)
table(bank$y)
head(bank)
summary(bank)
