library(tidyverse)
library(caret)
library(class)
library(rpart)
library(e1071)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)

wesbrook_dataset <- read_csv("http://jolej.linuxpl.info/Wesbrook.csv",
                             col_types = "ificifffffffffffffinnnnniifnnnn")

# Ta sekcja jest identyczna do obrobki danych z poprzednich laboratoriów 1-2,
# pomijam tu kroki eliminacji zmiennych z duzą liczbą NA values oraz wykrywania cech znaczacych. Zostało to wykonane w poprzednich laboratoriach które także załączę w folderze w razie wyjasnienia wybrania danej cechy.
# Konwersja daty
wesbrook_dataset <- wesbrook_dataset %>%
  mutate(INDUPDT = mdy(INDUPDT))

glimpse(wesbrook_dataset)

# Wstępna analiza zbioru
cat("\n Liczba i procent brakujących wartości według kolumn:\n")
missing_data <- data.frame(
  Variable = names(wesbrook_dataset),
  Count = colSums(is.na(wesbrook_dataset)),
  Percentage = round(colSums(is.na(wesbrook_dataset)) / nrow(wesbrook_dataset) * 100, 2)
)
print(missing_data)

# Analiza istotności zmiennych
wesbrook_dataset <- subset(
  wesbrook_dataset,
  !is.na(MOV_DWEL) &
    !is.na(HH_1PER) &
    !is.na(HH_2PER)
)

# Wstępna analiza zbioru, usuwamy tez MARITAL gdyz podczas poprzednich laboratoriów zostawiona zostala ta cecha tylko pod imputacje, a nie jest ona istotna w kontekście predykcji.
wesbrook_dataset <- subset(wesbrook_dataset,
                           select = -c(
                             ID, GRADYR1, MARITAL,
                             FACULTY1, DEPT1,
                             MAJOR1, FRSTYEAR,
                             BIGBLOCK, INDUPDT, EA
                           )
)

cat("\n Statystyki po wstępnym czyszczeniu danych:\n")
summary(wesbrook_dataset)

# Utworzenie zbioru Wesbrook2 z wybranymi zmiennymi
wesbrook2 <- wesbrook_dataset[c("TOTLGIVE", "AVE_INC", "DWEL_VAL",
                                names(wesbrook_dataset)[sapply(wesbrook_dataset, is.factor)]
)]

# Imputacja brakujących wartości
wesbrook2 <- wesbrook2 %>%
  mutate(AVE_INC = ifelse(is.na(AVE_INC), median(AVE_INC, na.rm = TRUE), AVE_INC))


# Normalizacja zmiennych numerycznych poprzez min-max scaling
wesbrook2 <- wesbrook2 %>%
  mutate(DWEL_VAL = (DWEL_VAL - min(DWEL_VAL)) / (max(DWEL_VAL) - min(DWEL_VAL)) *
    (1 - 0) + 0) %>%
  mutate(AVE_INC = (AVE_INC - min(AVE_INC)) / (max(AVE_INC) - min(AVE_INC)) *
    (1 - 0) + 0) %>%
  mutate(TOTLGIVE = (TOTLGIVE - min(TOTLGIVE)) / (max(TOTLGIVE) - min(TOTLGIVE)) *
    (1 - 0) + 0)

cat("\n Statystyki po normalizacji:\n")
summary(wesbrook2)

# Stworzenie zbiorów treningowych i testowych

# Zbiór z podzielonymi klasami oraz cechami
