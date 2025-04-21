library(tidyverse)


# Zadanie 1: Import danych
url <- "http://jolej.linuxpl.info/Wesbrook.csv"
wesbrook_dataset <- read_csv(url, col_types = "ificifffffffffffffinnnnniifnnnn")

glimpse(wesbrook_dataset)

# Konwersja daty
wesbrook_dataset <- wesbrook_dataset %>%
  mutate(INDUPDT = mdy(INDUPDT))

glimpse(wesbrook_dataset)

# Zadanie 2: Wstępna analiza zbioru
cat("\n Liczba i procent brakujących wartości według kolumn:\n")
missing_data <- data.frame(
  Variable = names(wesbrook_dataset),
  Count = colSums(is.na(wesbrook_dataset)),
  Percentage = round(colSums(is.na(wesbrook_dataset)) / nrow(wesbrook_dataset) * 100, 2)
)
print(missing_data)

# Zachowanie oryginalnej zmiennej WESBROOK do wizualizacji
wesbrook_dataset$WESBROOK_original <- wesbrook_dataset$WESBROOK

# Zadanie 3: Analiza istotności zmiennych
wesbrook_dataset <- subset(
  wesbrook_dataset,
  !is.na(MOV_DWEL) &
    !is.na(HH_1PER) &
    !is.na(HH_2PER)
)

# Usuwanie kolumn ktore uznaliśmy za zbędne w kontekście predykcji
wesbrook_dataset <- subset(wesbrook_dataset,
                           select = -c(ID, GRADYR1,
                                       FACULTY1, DEPT1,
                                       MAJOR1, FRSTYEAR,
                                       BIGBLOCK, EA,
                                       INDUPDT))

cat("\n Statystyki po wstępnym czyszczeniu danych:\n")
summary(wesbrook_dataset)