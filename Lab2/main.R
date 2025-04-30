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
library(caret)

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
wesbrook_features_train <- wesbrook_features[sample_index,]
wesbrook_features_test <- wesbrook_features[-sample_index,]

# Zbiory z klasami
wesbrook_labels_train <- as.factor(wesbrook_labels[sample_index,]$WESBROOK)
wesbrook_labels_test <- as.factor(wesbrook_labels[-sample_index,]$WESBROOK)

# Zliczemy liczby klas w zbiorach
table(wesbrook_labels_train)
table(wesbrook_labels_test)

# Zbiór z niepodzielonymi cechami i klasami
# Przygotowujemy pełny zbiór danych włącznie ze zmienną WESBROOK dla operacji na paczkach caret
wesbrook <- wesbrook2

# Postepujemy podobnie jak w przypadku zbiorów treningowych i testowych
formula_full_text <- paste("~ 0 +", paste(categorical_vars, collapse = " + "))
formula_full_obj <- as.formula(formula_full_text)

# Tworzymy macierz zmiennych dummy
dummy_matrix_full <- model.matrix(formula_full_obj, data = wesbrook)

wesbrook_numeric <- wesbrook %>%
  select(WESBROOK, TOTLGIVE, AVE_INC, DWEL_VAL)
wesbrook <- cbind(wesbrook_numeric, as.data.frame(dummy_matrix_full))

set.seed(1234)
sample_index <- sample(nrow(wesbrook), round(nrow(wesbrook) * 0.75),
                       replace = FALSE)
wesbrook_train <- wesbrook[sample_index,]
wesbrook_test <- wesbrook[-sample_index,]

table(wesbrook_train$WESBROOK)
table(wesbrook_test$WESBROOK)

# Budowa modeli

# K-najbliższych sąsiadów, wstepnie dajemy k=5, pozniej sprawdzimy dla pozostalych
cat("_____________K-najbliższych sąsiadów_____________\n")
wesbrook_knn_predictions <-
  knn(
    train = wesbrook_features_train,
    test = wesbrook_features_test,
    cl = wesbrook_labels_train, # cl to klasy
    k = 5
  )

# Macierz pomyłek do ktorej dajemy nasze przewidywania oraz etykiety
confusionMatrix(wesbrook_knn_predictions, wesbrook_labels_test)

# Walidacja krzyzowa K-krotna
kcv_knn <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  metric = "Accuracy",
  method = "knn",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
)

kcv_knn

# Przewidywanie na zbiorze testowym
kcv_knn_predictions <- predict(kcv_knn, wesbrook_test, type = "raw")
confusionMatrix(kcv_knn_predictions, wesbrook_test$WESBROOK)

# Walidacja krzyzowa metoda LGOCV dla losowego podzialu gdzie mamy wiele cech dla danej zmiennej
rcv_knn <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  metric = "Accuracy",
  method = "knn",
  trControl = trainControl(method = "LGOCV", p = .2, number = 10, verboseIter = TRUE),
)

rcv_knn

# Przewidywanie na zbiorze testowym
rcv_knn_predictions <- predict(rcv_knn, wesbrook_test, type = "raw")
confusionMatrix(rcv_knn_predictions, wesbrook_test$WESBROOK)

# Klasyfikator k-najbliższych sąsiadów, dla którego metoda k-krotnej walidacji krzyżowej dobrała parametr
# k=7, a metoda losowej walidacji krzyżowej także wybrała k=7. Osiągamy w obydwu przypadkach zbliżoną skuteczność — odpowiednio 71.43% i 72.1%. To wskazuje na stabilność modelu niezależnie od metody walidacji. W macierzy pomyłek widać, że większy problem stanowią błędne klasyfikacje próbek pozytywnych, co w kontekście zadania może prowadzić do utraty potencjalnych donacji. Nie wykonano analizy krzywej ROC z uwagi na ograniczenia techniczne lub formalne tego typu zadania klasyfikacyjnego.

cat("_____________Naiwny Bayes_____________\n")
# Naiwny klasyfikator Bayesa
nb <- naiveBayes(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  laplace = 1 # dodajemy 1 do każdej zmiennej aby uniknąć zer w liczniku, wygladzenie laplace'a
)

nb_predictions <- predict(nb, wesbrook_test, type = "class")
confusionMatrix(nb_predictions, wesbrook_test$WESBROOK)

nb_predictions_prob <- predict(nb, wesbrook_test, type = "raw")

# Obliczamy krzywą ROC na podstawie przewidywanych prawdopodobieństw, arg1: predykcja, arg2: etykiety
roc_pred <- prediction(nb_predictions_prob[, "Y"],
                       labels = wesbrook_test$WESBROOK
)

# measure = "tpr" oznacza true positive rate, a x.measure = "fpr" oznacza false positive rate
roc_pref <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_pref <- performance(roc_pred, measure = "auc") # auc to pole pod krzywą

plot(roc_pref, main = paste(
  "ROC Curve,
  AUC =", unlist(slot(auc_pref, "y.values"))),
     col = "blue", lwd = 3
)

# Linia symbolizujaca przypadkowe przewidywanie 50% prawdopodobieństwa
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# K-krotna walidacja krzyzowa
kcv_nb <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  metric = "Accuracy",
  method = "naive_bayes",
  tuneGrid = data.frame(
    usekernel = FALSE, # FALSE = Gaussian, TRUE = kernel density
    laplace = 1,
    adjust = 1
  ),
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
)

kcv_nb

kcv_nb_predictions <- predict(
  kcv_nb, wesbrook_test, type = "raw")
confusionMatrix(kcv_nb_predictions,
                wesbrook_test$WESBROOK)

# Ten parametr jest potrzebny do obliczenia krzywej ROC, type prob daje nam prawdopodobieństwa
kcv_nb_predictions_prob <- predict(
  kcv_nb, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = kcv_nb_predictions_prob[, "Y"], # prawdopodobieństwa dla klasy Y
  labels = wesbrook_test$WESBROOK
)

roc_pref <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_pref <- performance(roc_pred, measure = "auc") # auc to pole pod krzywą

plot(roc_pref, main = paste(
  "ROC Curve,
  AUC =", unlist(slot(auc_pref, "y.values"))),
     col = "blue", lwd = 3
)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Losowa walidacja krzyzowa
rcv_nb <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  metric = "Accuracy",
  method = "naive_bayes",
  tuneGrid = data.frame(
    usekernel = TRUE, # FALSE = Gaussian, TRUE = kernel density
    laplace = 1,
    adjust = 1
  ),
  trControl = trainControl(method = "LGOCV", p = .2, number = 10, verboseIter = TRUE),
)

rcv_nb

rcv_nb_predictions <- predict(
  rcv_nb, wesbrook_test, type = "raw")
confusionMatrix(rcv_nb_predictions,
                wesbrook_test$WESBROOK)

rcv_nb_predictions_prob <- predict(
  rcv_nb, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = rcv_nb_predictions_prob[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_pref <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_pref <- performance(roc_pred, measure = "auc") # auc to pole pod krzywą

plot(roc_pref, main = paste(
  "ROC Curve,
  AUC =", unlist(slot(auc_pref, "y.values"))),
     col = "blue", lwd = 3
)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Nie znaleziono klasyfikatora, który miałby dokładność lepszą niż 50 %, chyba ze zmienilibysmy usekernel = TRUE, # FALSE = Gaussian, TRUE = kernel density na TRUE wtedy dla rozkladu gestosci osiagamy wynik 55% i mamy znacznie wieksza czulosc na negatywy,  oznacza to że ten klasyfikator nie jest wogóle lepszy od predykcji losowej. Z macierzy pomyłek można zauważyć, że prawie dla wszystkich próbek klasyfikator predykuje klasę "Y", czyli, że jest to osoba która mogłaby dać donacje. Klasyfikator jest dla nas bezużyteczny, według niego powinniśmy wysłać prawie do każdego zachęte odnośnie donacji, co byłoby stratą czasu. Wyjatkiem jest ten dla kernel TRUE ktory wykrywa poprawnie wszystkie negatywy ale ledwo ktore pozytywy wykrywa.

# Drzewa decyzyjne
cat("_____________Drzewa decyzyjne_____________\n")
# dt <- rpart(
#   WESBROOK ~ ., # wszystkie zmienne
#   data = wesbrook_train,
#   method = "class",
#   cp = 0.00002 # parametr cp to minimalny wzrost jakości podziału, aby utworzyć nowy węzeł czyli jeśli nie ma poprawy to nie dzielimy dalej
# )
#
# rpart.plot(dt)
#
# dt_predictions <- predict(dt, wesbrook_test, type = "class")
# confusionMatrix(dt_predictions, wesbrook_test$WESBROOK)

# Testujemy różne wartości cp, aby znaleźć najlepszą wartość, uzywamy 50 jako warotsci kontrolnej czyli 50 podziałów
dt_grid <- train(
  WESBROOK ~ .,
  data = wesbrook_train,
  method = "rpart",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 100),
  tuneGrid = data.frame(cp = seq(0.00001, 0.001, length.out = 50)),
)

# Wyświetl najlepszą wartość cp
print(dt_grid$bestTune)

dt_predictions <- predict(dt_grid, wesbrook_test)
confusionMatrix(dt_predictions, wesbrook_test$WESBROOK)

best_tree <- dt_grid$finalModel
rpart.plot(best_tree)

# Krzywa ROC
dt_predictions_prob <- predict(dt_grid, wesbrook_test, type = "prob")
roc_pred <- prediction(
  predictions = dt_predictions_prob[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_pref <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_pref <- performance(roc_pred, measure = "auc") # auc to pole pod krzywą

plot(roc_pref, main = paste(
  "ROC Curve,
  AUC =", unlist(slot(auc_pref, "y.values"))),
     col = "blue", lwd = 3
)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# K krotna walidacja krzyzowa
kcv_dt <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  method = "rpart",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(cp = seq(0.00001, 0.001, length.out = 50))
)

kcv_dt

kcv_dt_predictions <- predict(kcv_dt, wesbrook_test, type = "raw")
confusionMatrix(kcv_dt_predictions, wesbrook_test$WESBROOK)

kcv_dt_predictions_prob <- predict(kcv_dt, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = kcv_dt_predictions_prob[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_perf <- performance(roc_pred, measure = "auc") # auc to pole pod krzywą

plot(roc_perf, main = paste("ROC CURVE, ",
                            "AUC:", unlist(slot(auc_perf, "y.values"))), col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Losowa walidacja krzyzowa
rcv_dt <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  method = "rpart",
  metric = "Accuracy",
  trControl = trainControl(method = "LGOCV", p = .2, number = 10),
  tuneGrid = data.frame(cp = seq(0.00001, 0.001, length.out = 50))
)

rcv_dt

rcv_dt_predictions <- predict(rcv_dt, wesbrook_test, type = "raw")
confusionMatrix(rcv_dt_predictions, wesbrook_test$WESBROOK)

rcv_dt_predictions_prob <- predict(rcv_dt, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = rcv_dt_predictions_prob[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_perf <- performance(roc_pred, measure = "auc")

plot(roc_perf, main = paste("ROC CURVE, ",
                            "AUC:", unlist(slot(auc_perf, "y.values"))), col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Podczas eksperymentów z parametrem cp zaobserwowano, że dla wartości większych niż cp > 0.0001, algorytm ogranicza się do użycia tylko jednej zmiennej decyzyjnej — TOTLGIVE, reprezentującej łączną kwotę przekazanych darowizn. Dopiero przy zmniejszeniu wartości cp, drzewo zaczyna rozgałęziać się głębiej i uwzględniać także inne cechy, takie jak AVE_INC, co pozwala lepiej odwzorować złożoność danych.
#
# Aby dobrać optymalny parametr cp, przeprowadzono siatkę poszukiwań (ang. grid search) w zakresie 0.00001–0.001 z 50 podziałami oraz zastosowano 100-krotną walidację krzyżową. Uzyskany w ten sposób model osiągnął wysoką dokładność na poziomie 96.1%, a najlepszy znaleziony parametr to cp ≈ 0.00098.
#
# Dodatkowo przeprowadzono dwa typy walidacji krzyżowej:
# + K-krotną walidację krzyżową (5-fold)
# + Losową walidację krzyżową (LGOCV) z podziałem 80/20 powtarzaną 10 razy
#
# Obie metody niezależnie wybrały tę samą optymalną wartość cp = 0.001. Wszystkie trzy modele (bazowy, k-krotny i losowy) osiągnęły identyczne wyniki:
#
# Accuracy: 0.9606
# Sensitivity (Recall): 0.9789
# Specificity: 0.9435
# Balanced Accuracy: 0.9612
# Kappa: 0.9213
# AUC (ROC): ≈ 0.96
#
# To oznacza, że klasyfikator drzewa decyzyjnego jest nie tylko precyzyjny, ale również stabilny, niezależnie od metody walidacji. Krzywa ROC wykazuje bardzo dobrą separację klas, a AUC potwierdza wysoką jakość klasyfikacji.

# Random Forest
cat("_____________Random Forest_____________\n")
rf <- randomForest(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  ntrees = 10 # liczba drzew
)

rf_predictions <- predict(rf, wesbrook_test, type = "class")
confusionMatrix(rf_predictions, wesbrook_test$WESBROOK)

# Krzywa ROC
rf_predictions_probabilities <- predict(rf, wesbrook_test, type = "prob")
roc_pred <- prediction(
  predictions = rf_predictions_probabilities[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_perf <- performance(roc_pred, measure = "auc")

plot(roc_perf, main = paste("ROC CURVE, ",
                            "AUC:", unlist(slot(auc_perf, "y.values"))), col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# K - krotna walidacja krzyzowa
kcv_rf <- train(
  WESBROOK ~ .,
  data = wesbrook_train,
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method = "cv", number = 10)
)

kcv_rf

kcv_rf_predictions <- predict(kcv_rf, wesbrook_test, type = "raw")
confusionMatrix(kcv_rf_predictions, wesbrook_test$WESBROOK)

kcv_rf_predictions_prob <- predict(kcv_rf, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = kcv_dt_predictions_prob[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_perf <- performance(roc_pred, measure = "auc")

plot(roc_perf, main = paste("ROC CURVE, ",
                            "AUC:", unlist(slot(auc_perf, "y.values"))), col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Losowa walidacja krzyzowa
rcv_rf <- train(
  WESBROOK ~ .,
  data = wesbrook_train,
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method = "LGOCV", p = .2, number = 100)
)

rcv_rf

rcv_rf_predictions <- predict(rcv_rf, wesbrook_test, type = "raw")
confusionMatrix(rcv_dt_predictions, wesbrook_test$WESBROOK)

rcv_rf_predictions_probabilities <- predict(rcv_rf, wesbrook_test, type = "prob")

roc_pred <- prediction(
  predictions = rcv_rf_predictions_probabilities[, "Y"],
  labels = wesbrook_test$WESBROOK
)

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")
auc_perf <- performance(roc_pred, measure = "auc")

plot(roc_perf, main = paste("ROC CURVE, ",
                            "AUC:", unlist(slot(auc_perf, "y.values"))), col = "red", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Przy samodzielnych eksperymentach z parametrem ntrees wynika, że nie ma on wielkiego wpływu na dokładność. Algorytm random forest przy parametrze ntrees = 10 uzyskał dokładnośc na poziomie 96 %. Praktycznie nie myli się przy próbkach osób, które dały donacje w macierzy pomyłek. Większość przypadków pomyłek to predykcja tak, dla osób, które nie dały donacji większej niż 1000, są to 22 przypadki wśród 354 negatywnych, co nie byłoby ogromnną stratą czasu w przypadku zachęcenia tych osób. K-krotna walidacja oraz losowa walidacja uzyskały również dokładności na poziomie 96 % i podobne pomyłki jak model, z którym eksperymentowano własnoręcznie. Wszystkie modele uzyskały bardzo wysokie pole pod wykresem ROC = ok. 0.98 - 0.99 jest to bardzo dobry wynik.

# XGBoost
xgb <- train(
  WESBROOK ~ ., # wszystkie zmienne
  data = wesbrook_train,
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0.01,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

xgb_predictions <- predict(xgb, wesbrook_test, type = "raw")
confusionMatrix(xgb_predictions, wesbrook_test$WESBROOK)