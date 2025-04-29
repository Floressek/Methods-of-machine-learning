# Wymagane biblioteki
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
# pomijam tu kroki eliminacji zmiennych z duzą liczbą NA values oraz wykrywania cech znaczacych. Zostało to wykonane w poprzednich laboratoriach, które także załączę w folderze w razie wyjasnienia wybrania danej cechy.

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
# Zbiory z podzielonymi klasami oraz cechami

# Zbiór z klasami
wesbrook_labels <- wesbrook2 %>% select(WESBROOK)
# Zbiór z cechami, traktujemy go jako data.frame
wesbrook_features <- wesbrook2 %>% select(-WESBROOK)
wesbrook_features <- as.data.frame(wesbrook_features)

# Tworzymy formułę zawierającą wszystkie zmienne kategoryczne
categorical_vars <- c("PARENT", "CHILD", "SPOUSE", "SEX", "FACSTAFF",
                      "ATHLTCS", "OTHERACT", "PROV")
# ~ 0 + oznacza ze moduł nie ma interceptu, a zmienne kategoryczne są traktowane jako zmienne wskaźnikowe (dummy variables) czyli np jak SEX ma F i M to otrzymamt 2 kolumny SEXM oraz SEXF
formula_text <- paste("~ 0 +", paste(categorical_vars, collapse = " + "))
formula_obj <- as.formula(formula_text)

# Pakiet dummies nie dzialał wiec zastosujemy model.matrix jako alternatywę
dummy_matrix <- model.matrix(formula_obj, data = wesbrook_features)
# Idea tej macierzy jest taka, że dla każdej zmiennej kategorycznej tworzy kolumny z wartościami 0 i 1, które wskazują, czy dany wiersz należy do danej kategorii.

# Łączymy macierz dummy z oryginalnym zbiorem cech
wesbrook_features_numeric <- wesbrook_features %>%
  select(TOTLGIVE, AVE_INC, DWEL_VAL)
wesbrook_features <- cbind(wesbrook_features_numeric, as.data.frame(dummy_matrix))

# Zeby zapewnic deterministyczność wyników, ustawiamy ziarno
set.seed(1234) #FIXME: test different seeds for data splitting
# Ustalamy proporcje podziału
sample_index <- sample(nrow(wesbrook_features), round(nrow(wesbrook_features) * 0.75), replace = FALSE)

# Tworzymy zbiory treningowe i testowe
wesbrook_features_train <- wesbrook_features[sample_index, ]
wesbrook_features_test <- wesbrook_features[-sample_index, ]

# Zbiory z klasami
wesbrook_labels_train <- as.factor(wesbrook_labels[sample_index, ]$WESBROOK)
wesbrook_labels_test <- as.factor(wesbrook_labels[-sample_index, ]$WESBROOK)

# Zliczemy liczby klas w zbiorach
table(wesbrook_labels_train)
table(wesbrook_labels_test)
