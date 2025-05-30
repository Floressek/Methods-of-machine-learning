library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(gbm)
library(xgboost)  # Dodany XGBoost
library(ROCR)
library(pROC)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(knitr)
# Sprawdzenie i instalacja smotefamily jeśli potrzebne
if (!require(smotefamily, quietly = TRUE)) {
  install.packages("smotefamily")
  library(smotefamily)
}

# Ustawienie tematu dla wykresów
theme_set(theme_minimal())

# ===========================
# 1. ŁADOWANIE I PRZETWARZANIE DANYCH
# ===========================

tryCatch({
  data <- read.csv("Project/data/heart_2020_cleaned.csv")
}, error = function(e) {
  stop("Nie udało się załadować danych. Sprawdź ścieżkę pliku.")
})

summary(data)

# Oczyszczanie nazw kolumn
names(data) <- gsub("[^a-zA-Z0-9_]", "", names(data))

cat("Wymiary danych:", nrow(data), "wierszy,", ncol(data), "kolumn\n")
head(data)
str(data)

# Słownik danych (wnioskowany)
cat("=== SŁOWNIK DANYCH ===\n")
cat("1. HeartDisease: Choroby serca (Tak/Nie)\n")
cat("2. BMI: Wskaźnik masy ciała (numeryczny)\n")
cat("3. Smoking: Palenie tytoniu (Tak/Nie)\n")
cat("4. AlcoholDrinking: Nadużywanie alkoholu (Tak/Nie)\n")
cat("5. Stroke: Udar mózgu (Tak/Nie)\n")
cat("6. PhysicalHealth: Dni złego stanu fizycznego w ostatnich 30 dniach (0-30)\n")
cat("7. MentalHealth: Dni złego stanu psychicznego w ostatnich 30 dniach (0-30)\n")
cat("8. DiffWalking: Trudności z chodzeniem (Tak/Nie)\n")
cat("9. Sex: Płeć (Kobieta/Mężczyzna)\n")
cat("10. AgeCategory: Kategoria wiekowa\n")
cat("11. Race: Rasa/pochodzenie\n")
cat("12. Diabetic: Cukrzyca (4 kategorie)\n")
cat("13. PhysicalActivity: Aktywność fizyczna (Tak/Nie)\n")
cat("14. GenHealth: Ogólny stan zdrowia\n")
cat("15. SleepTime: Czas snu (godziny)\n")
cat("16. Asthma: Astma (Tak/Nie)\n")
cat("17. KidneyDisease: Choroby nerek (Tak/Nie)\n")
cat("18. SkinCancer: Rak skóry (Tak/Nie)\n")

# ===========================
# 2. OCZYSZCZANIE DANYCH
# ===========================
data_clean <- data

cat("Początkowa liczba wierszy:", nrow(data_clean), "\n")

# Obsługa wartości NA i problematycznych stringów
cat("\n=== WYKRYWANIE I OBSŁUGA PROBLEMATYCZNYCH WARTOŚCI ===\n")

potential_numerical_cols <- c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")
potential_numerical_cols <- intersect(potential_numerical_cols, names(data_clean))

# Konwersja stringów NA na R NA
cat("Krok 1: Konwersja stringowych reprezentacji NA...\n")
na_strings_to_check <- c("NA", "N/A", "na", "n/a", "", " ", "NULL", "Null", "null", "Missing", "?", ".", "Unknown", "Not Specified")
for (col_name in names(data_clean)) {
  if (is.character(data_clean[[col_name]])) {
    na_before <- sum(is.na(data_clean[[col_name]]))
    data_clean[[col_name]][trimws(data_clean[[col_name]]) %in% na_strings_to_check] <- NA
    na_after <- sum(is.na(data_clean[[col_name]]))
    if (na_after > na_before) {
      cat("  Kolumna '", col_name, "': Przekonwertowano ", na_after - na_before, " stringowych NA.\n")
    }
  }
}

# Konwersja wartości nieskończonych na NA
cat("\nKrok 2: Konwersja wartości nieskończonych na NA...\n")
if (length(potential_numerical_cols) > 0) {
  for (num_col in potential_numerical_cols) {
    if (is.numeric(data_clean[[num_col]])) {
      inf_before <- sum(is.infinite(data_clean[[num_col]]))
      if (inf_before > 0) {
        data_clean[[num_col]][is.infinite(data_clean[[num_col]])] <- NA
        cat("  Kolumna '", num_col, "': Przekonwertowano ", inf_before, " wartości nieskończonych.\n")
      }
    }
  }
}

# Raportowanie i usuwanie NA
cat("\nKrok 3: Raportowanie końcowych liczb NA i aplikacja na.omit()...\n")
na_counts_final <- sapply(data_clean, function(x) sum(is.na(x)))
total_na_values_final <- sum(na_counts_final)

if (total_na_values_final > 0) {
  cat("Całkowita liczba wartości NA:", total_na_values_final, "\n")
  print("Liczba NA w kolumnach przed na.omit():")
  print(na_counts_final[na_counts_final > 0])

  original_rows <- nrow(data_clean)
  data_clean <- na.omit(data_clean)
  rows_removed <- original_rows - nrow(data_clean)
  cat("\nZastosowano na.omit(). Usunięto", rows_removed, "wierszy zawierających NA.\n")
  if (rows_removed > 0 && (rows_removed / original_rows) > 0.1) {
    warning(paste("Ponad 10% wierszy (", rows_removed, ") usunięto z powodu NA."))
  }
}
cat("Dane po obsłudze NA:", nrow(data_clean), "wierszy\n")

# Definiowanie cech numerycznych i kategorycznych
numerical_features <- c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")
numerical_features <- intersect(numerical_features, names(data_clean))

all_columns <- names(data_clean)
categorical_features <- setdiff(all_columns, c(numerical_features, "HeartDisease"))

cat("\n--- KONWERSJA KOLUMN NA ODPOWIEDNIE TYPY FAKTORÓW ---\n")

# Konwersja zmiennych binarnych na faktory
binary_yes_no_cols <- c("Smoking", "AlcoholDrinking", "Stroke", "DiffWalking",
                        "PhysicalActivity", "Asthma", "KidneyDisease", "SkinCancer")
binary_yes_no_cols <- intersect(binary_yes_no_cols, names(data_clean))

for (col in binary_yes_no_cols) {
  unique_vals <- unique(data_clean[[col]])
  expected_vals <- c("No", "Yes")
  if (!all(unique_vals %in% expected_vals)) {
    warning(paste("Kolumna '", col, "' zawiera wartości inne niż 'No', 'Yes': ",
                  paste(setdiff(unique_vals, expected_vals), collapse = ", ")))
  }
  data_clean[[col]] <- factor(data_clean[[col]], levels = c("No", "Yes"))
  cat("Przekonwertowano '", col, "' na faktor.\n")
}

# SPECJALNA OBSŁUGA DLA DIABETIC - ma 4 kategorie
if ("Diabetic" %in% names(data_clean)) {
  cat("\nSpecjalna obsługa kolumny 'Diabetic' z wieloma kategoriami...\n")
  diabetic_unique <- unique(data_clean$Diabetic)
  cat("Znalezione wartości w Diabetic:", paste(diabetic_unique, collapse = ", "), "\n")

  # Zliczenie każdej kategorii
  diabetic_counts <- table(data_clean$Diabetic)
  print("Rozkład kategorii Diabetic:")
  print(diabetic_counts)

  # Konwersja na faktor z wszystkimi kategoriami
  diabetic_levels <- c("No", "No, borderline diabetes", "Yes", "Yes (during pregnancy)")
  data_clean$Diabetic <- factor(data_clean$Diabetic, levels = diabetic_levels)
  cat("Przekonwertowano 'Diabetic' na faktor z", length(diabetic_levels), "poziomami.\n")
}

# Płeć na faktor
if ("Sex" %in% names(data_clean)) {
  data_clean$Sex <- factor(data_clean$Sex, levels = c("Female", "Male"))
  cat("Przekonwertowano 'Sex' na faktor.\n")
}

# Kategorie wiekowe na faktor
age_levels_ordered <- c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                        "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older")
if ("AgeCategory" %in% names(data_clean)) {
  data_clean$AgeCategory <- factor(data_clean$AgeCategory, levels = age_levels_ordered, ordered = TRUE)
  cat("Przekonwertowano 'AgeCategory' na faktor.\n")
}

# Rasa na faktor
if ("Race" %in% names(data_clean)) {
  data_clean$Race <- factor(data_clean$Race)
  cat("Przekonwertowano 'Race' na faktor.\n")
}

# Konwersja ogólnego stanu zdrowia na faktor
gen_health_levels_ordered <- c("Poor", "Fair", "Good", "Very good", "Excellent")
if ("GenHealth" %in% names(data_clean)) {
  data_clean$GenHealth <- factor(data_clean$GenHealth, levels = gen_health_levels_ordered, ordered = TRUE)
  cat("Przekonwertowano 'GenHealth' na faktor.\n")
}

# Konwersja zmiennej docelowej HeartDisease na faktor
if ("HeartDisease" %in% names(data_clean)) {
  data_clean$HeartDisease <- factor(data_clean$HeartDisease, levels = c("No", "Yes"))
  cat("Przekonwertowano 'HeartDisease' (zmienna docelowa) na faktor.\n")
} else {
  stop("KRYTYCZNE: Zmienna docelowa 'HeartDisease' nie została znaleziona.")
}

# Finalna kontrola NA po konwersji faktorów
na_after_factors <- sapply(data_clean, function(x) sum(is.na(x)))
if (any(na_after_factors > na_counts_final[names(na_after_factors)])) {
  cat("\nOstrzeżenie: Dodatkowe NA wprowadzone podczas konwersji faktorów.\n")
  print("Liczba NA w kolumnach PO konwersji faktorów:")
  print(na_after_factors[na_after_factors > 0])
  original_rows_after_factors <- nrow(data_clean)
  data_clean <- na.omit(data_clean)
  cat("Zastosowano na.omit() ponownie. Usunięto", original_rows_after_factors - nrow(data_clean), "wierszy.\n")
}

# Weryfikacja końcowej struktury danych
cat("\n--- KOŃCOWA KONTROLA STRUKTURY OCZYSZCZONYCH DANYCH ---\n")
cat("Dostępne kolumny po oczyszczeniu:\n"); print(names(data_clean))
cat("Końcowe cechy numeryczne:\n"); print(numerical_features)
cat("Końcowe cechy kategoryczne (bez zmiennej docelowej):\n"); print(categorical_features)
cat("Struktura data_clean:\n")
summary(data_clean)

# ===========================
# 3. EKSPLORACYJNA ANALIZA DANYCH
# ===========================

if (nrow(data_clean) > 0) {
  # Rozkład zmiennej docelowej
  target_plot <- ggplot(data_clean, aes(x = HeartDisease, fill = HeartDisease)) +
    geom_bar(alpha = 0.8) +
    geom_text(stat = 'count', aes(label = paste0(after_stat(count), '\n',
                                                 scales::percent(after_stat(count) / sum(after_stat(count))))),
              vjust = -0.5, color = "black", fontface = "bold") +
    scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
    labs(title = "Rozklad zmiennej docelowej (Choroby serca)",
         x = "Choroby serca",
         y = "Liczba") +
    theme(legend.position = "none")
  print(target_plot)

  # Statystyki opisowe cech numerycznych
  if (length(numerical_features) > 0) {
    cat("\n=== STATYSTYKI OPISOWE CECH NUMERYCZNYCH ===\n")
    print(summary(data_clean[, numerical_features, drop = FALSE]))
  }

  # Mapa korelacji dla cech numerycznych
  if (length(numerical_features) > 1 && nrow(data_clean[, numerical_features, drop = FALSE]) > 1) {
    numerical_data_for_cor <- data_clean[, numerical_features, drop = FALSE]
    numerical_data_for_cor <- na.omit(numerical_data_for_cor)

    if (nrow(numerical_data_for_cor) > 1) {
      correlation_matrix <- cor(numerical_data_for_cor, use = "complete.obs")
      corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
               tl.cex = 0.8, tl.col = "black", addCoef.col = "black", number.cex = 0.7,
               title = "Mapa korelacji cech numerycznych", mar = c(0, 0, 1, 0))
    }
  }

  # Wykresy rozkładów dla cech numerycznych
  if (length(numerical_features) > 0) {
    plot_list_num <- list()
    for (i in seq_along(numerical_features)) {
      feature <- numerical_features[i]
      p <- ggplot(data_clean, aes_string(x = feature, fill = "HeartDisease")) +
        geom_density(alpha = 0.7) +
        scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
        labs(title = paste("Rozklad", feature), x = feature, y = "Gestosc")
      plot_list_num[[i]] <- p
    }
    if (length(plot_list_num) > 0) grid.arrange(grobs = plot_list_num, ncol = 2)
  }

  # Analiza cech kategorycznych
  if (length(categorical_features) > 0) {
    plot_list_cat <- list()
    for (i in seq_along(categorical_features)) {
      feature <- categorical_features[i]
      p <- ggplot(data_clean, aes_string(x = feature, fill = "HeartDisease")) +
        geom_bar(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
        labs(title = paste("Rozklad", feature), x = feature, y = "Liczba") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      plot_list_cat[[i]] <- p
    }
    if (length(plot_list_cat) > 0) grid.arrange(grobs = plot_list_cat, ncol = min(2, length(plot_list_cat)))
  }

  # SPECJALNA ANALIZA DIABETIC vs HEART DISEASE
  if ("Diabetic" %in% names(data_clean)) {
    cat("\n=== ANALIZA DIABETIC vs HEART DISEASE ===\n")

    # Tabela krzyżowa
    diabetic_heart_table <- table(data_clean$Diabetic, data_clean$HeartDisease)
    cat("Tabela krzyzowa Diabetic vs HeartDisease:\n")
    print(diabetic_heart_table)

    # Procenty w każdej kategorii cukrzycy
    diabetic_heart_prop <- prop.table(diabetic_heart_table, margin = 1) * 100
    cat("\nProcent chorob serca w kazdej kategorii cukrzycy:\n")
    print(round(diabetic_heart_prop, 2))

    # Wykres słupkowy - procenty
    diabetic_plot_prop <- ggplot(data_clean, aes(x = Diabetic, fill = HeartDisease)) +
      geom_bar(position = "fill", alpha = 0.8) +
      scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Procent chorob serca wedlug statusu cukrzycy",
           x = "Status cukrzycy",
           y = "Procent",
           fill = "Choroby serca") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(diabetic_plot_prop)

    # Statystyki opisowe
    cat("\n=== KLUCZOWE STATYSTYKI ===\n")
    for (diabetic_cat in levels(data_clean$Diabetic)) {
      subset_data <- data_clean[data_clean$Diabetic == diabetic_cat,]
      heart_yes_count <- sum(subset_data$HeartDisease == "Yes")
      total_count <- nrow(subset_data)
      percentage <- round((heart_yes_count / total_count) * 100, 2)

      cat(sprintf("%-30s: %6d/%6d (%5.2f%%) ma choroby serca\n",
                  diabetic_cat, heart_yes_count, total_count, percentage))
    }
  }
} else {
  cat("Dane są puste po oczyszczaniu. Pomijanie EDA.\n")
}

# ===========================
# 4. PRZYGOTOWANIE MODELI
# ===========================
if (nrow(data_clean) == 0) {
  stop("Brak danych do przygotowania modeli.")
}

set.seed(42)
# Stratyfikowany podział na zbiory treningowy i testowy
cat("Podział oczyszczonych danych na zbiory treningowy i testowy (75/25)...\n")
train_index <- createDataPartition(data_clean$HeartDisease, p = 0.75, list = FALSE)
initial_train_data <- data_clean[train_index,]
test_data <- data_clean[-train_index,]

cat("Rozmiar początkowego zbioru treningowego:", nrow(initial_train_data), "\n")
cat("Rozkład klas w początkowym zbiorze treningowym:\n")
print(table(initial_train_data$HeartDisease))
cat("Rozmiar zbioru testowego:", nrow(test_data), "\n")

# ===========================
# ZASTOSOWANIE SMOTE DO BALANSOWANIA KLAS
# ===========================
cat("\n=== ZASTOSOWANIE SMOTE DO BALANSOWANIA KLAS ===\n")

set.seed(123)
cat("Stosowanie SMOTE do balansowania klas...\n")
cat("Przed balansowaniem - rozkład klas:\n")
print(table(initial_train_data$HeartDisease))

# Przygotowanie danych dla SMOTE
# SMOTE wymaga danych numerycznych, więc przekonwertujemy faktory
train_for_smote <- initial_train_data

# Konwersja faktorów na numeryczne (oprócz zmiennej docelowej)
for (col in names(train_for_smote)) {
  if (col != "HeartDisease" && is.factor(train_for_smote[[col]])) {
    train_for_smote[[col]] <- as.numeric(train_for_smote[[col]])
  }
}

# Konwersja zmiennej docelowej na numeryczną dla SMOTE
y_numeric_smote <- ifelse(train_for_smote$HeartDisease == "Yes", 1, 0)
X_for_smote <- train_for_smote[, !names(train_for_smote) %in% "HeartDisease"]

# Zastosowanie SMOTE
tryCatch({
  cat("Stosowanie SMOTE z biblioteki smotefamily...\n")
  smote_result <- SMOTE(X_for_smote, y_numeric_smote, K = 5, dup_size = 2)

  # Przygotowanie danych po SMOTE
  smoted_data <- smote_result$data

  # Konwersja z powrotem na faktory
  # Przywrócenie oryginalnych nazw kolumn i typów
  for (col in names(X_for_smote)) {
    if (col %in% names(initial_train_data)) {
      original_col <- initial_train_data[[col]]
      if (is.factor(original_col)) {
        # Konwersja z numerycznej z powrotem na faktor
        levels_original <- levels(original_col)
        smoted_data[[col]] <- factor(levels_original[smoted_data[[col]]], levels = levels_original)
      }
    }
  }

  # Dodanie zmiennej docelowej
  target_col <- smoted_data[["class"]]
  smoted_data$HeartDisease <- factor(ifelse(target_col == 1, "Yes", "No"), levels = c("No", "Yes"))
  smoted_data <- smoted_data[, !names(smoted_data) %in% "class"]

  train_smote <- smoted_data
  cat("SMOTE z idealnym balansem 50:50 zakończone pomyślnie!\n")

}, error = function(e) {
  cat("Błąd z SMOTE, stosowanie prostego oversampling/undersampling...\n")
  cat("Błąd:", e$message, "\n")

  # Fallback - proste over/undersampling z balansem 50:50
  minority_class <- initial_train_data[initial_train_data$HeartDisease == "Yes",]
  majority_class <- initial_train_data[initial_train_data$HeartDisease == "No",]

  n_minority <- nrow(minority_class)
  n_majority <- nrow(majority_class)

  # Strategia balansowania
  target_minority <- round(n_minority * 2)  # Podwojenie klasy mniejszościowej
  target_majority <- round(target_minority * 1.5)  # Stosunek 60:40

  # Oversampling klasy mniejszościowej
  set.seed(123)
  minority_indices <- sample(1:n_minority, target_minority, replace = TRUE)
  minority_oversampled <- minority_class[minority_indices,]

  # Undersampling klasy większościowej
  set.seed(456)
  majority_indices <- sample(1:n_majority, min(target_majority, n_majority), replace = FALSE)
  majority_undersampled <- majority_class[majority_indices,]

  # Łączenie i tasowanie
  train_smote <<- rbind(minority_oversampled, majority_undersampled)
  set.seed(789)
  train_smote <<- train_smote[sample(nrow(train_smote)),]

  cat("Zastosowano alternatywne balansowanie klas.\n")
})

cat("Po balansowaniu - rozkład klas:\n")
print(table(train_smote$HeartDisease))

# Wykres rozkładu zmiennej docelowej po SMOTE
smote_target_plot <- ggplot(train_smote, aes(x = HeartDisease, fill = HeartDisease)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', aes(label = paste0(after_stat(count), '\n',
                                               scales::percent(after_stat(count) / sum(after_stat(count))))),
            vjust = -0.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
  labs(title = "Rozklad zmiennej docelowej po SMOTE",
       x = "Choroby serca", y = "Liczba") +
  theme(legend.position = "none")
print(smote_target_plot)

# Przygotowanie macierzy cech i wektorów
train_data <- train_smote
X_train <- train_data[, !names(train_data) %in% "HeartDisease"]
X_test <- test_data[, !names(test_data) %in% "HeartDisease"]

for (col in names(X_train)) {
  # Tylko jeśli w X_train to zwykły factor, a w X_test ordered
  if (is.factor(X_train[[col]]) &&
    !is.ordered(X_train[[col]]) &&
    is.ordered(X_test[[col]])) {
    X_test[[col]] <- factor(as.character(X_test[[col]]), levels = levels(X_train[[col]]))
  }
  # Na wszelki wypadek - zawsze wymuszamy te same poziomy
  if (is.factor(X_train[[col]])) {
    X_test[[col]] <- factor(X_test[[col]], levels = levels(X_train[[col]]))
  }
}

# Upewniamy się, że kolejność kolumn się zgadza:
X_test <- X_test[, names(X_train)]

y_train_factor <- train_data$HeartDisease
y_test_factor <- test_data$HeartDisease

y_train_numeric <- ifelse(y_train_factor == "Yes", 1, 0)
y_test_numeric <- ifelse(y_test_factor == "Yes", 1, 0)

cat("\nWymiary X_train (po SMOTE):", dim(X_train), "\n")
cat("Wymiary X_test:", dim(X_test), "\n")

cat("Typy kolumn w X_train:\n")
print(sapply(X_train, class))
cat("Typy kolumn w X_test:\n")
print(sapply(X_test, class))


# ===========================
# 5. TRENOWANIE I EWALUACJA MODELI
# ===========================
# Funkcja do ewaluacji modelu
evaluate_model <- function(model, X_test_data, y_test_data_numeric, model_name_str) {
  y_test_data_numeric <- as.numeric(y_test_data_numeric)

  raw_predictions_numeric <- NULL
  probabilities_output <- NULL

  if (inherits(model, "glm")) {
    probabilities_output <- predict(model, X_test_data, type = "response")
    # Dodajemy progowanie dla regresji logistycznej
    raw_predictions_numeric <- ifelse(probabilities_output > 0.5, 1, 0)
  } else if (inherits(model, "randomForest")) {
    predicted_class_factor <- predict(model, X_test_data, type = "response")
    raw_predictions_numeric <- ifelse(predicted_class_factor == "Yes", 1, 0)
    prob_matrix <- predict(model, X_test_data, type = "prob")
    if ("Yes" %in% colnames(prob_matrix)) {
      probabilities_output <- prob_matrix[, "Yes"]
    } else {
      probabilities_output <- prob_matrix[, 2]
    }
  } else if (inherits(model, "rpart")) {
    predicted_class_factor <- predict(model, X_test_data, type = "class")
    raw_predictions_numeric <- ifelse(predicted_class_factor == "Yes", 1, 0)
    prob_matrix <- predict(model, X_test_data, type = "prob")
    if ("Yes" %in% colnames(prob_matrix)) {
      probabilities_output <- prob_matrix[, "Yes"]
    } else {
      probabilities_output <- prob_matrix[, 2]
    }
    # model gbm -> is a generalized boosted model FIXME try the plot.it = TRUE
  } else if (inherits(model, "gbm")) {
    best_iter <- tryCatch(gbm.perf(model, plot.it = FALSE, method = "OOB"),
                          error = function(e) model$n.trees)
    if (is.null(best_iter) ||
      !is.numeric(best_iter) ||
      best_iter < 1) {
      best_iter <- model$n.trees
    }
    probabilities_output <- predict(model, X_test_data, n.trees = best_iter, type = "response")
    raw_predictions_numeric <- ifelse(probabilities_output > 0.5, 1, 0)
  } else if (inherits(model, "xgb.Booster")) {
    if (is.data.frame(X_test_data)) {
      # Konwersja zmiennych factor na zmienne numeryczne
      X_test_numeric <- X_test_data
      factor_cols <- sapply(X_test_numeric, is.factor)
      X_test_numeric[factor_cols] <- lapply(X_test_numeric[factor_cols], as.numeric)
      test_matrix <- as.matrix(X_test_numeric) # Konwersja do macierzy
    } else {
      test_matrix <- as.matrix(X_test_data)
    }
    probabilities_output <- predict(model, test_matrix)
    raw_predictions_numeric <- ifelse(probabilities_output > 0.5, 1, 0)
  } else {
    warning(paste("Model", model_name_str, "nie jest obsługiwany w tej funkcji ewaluacji."))
    raw_predictions_numeric <- rep(0, nrow(X_test_data))
    probabilities_output <- rep(0.5, nrow(X_test_data))
  }
  # Kontrola jakości predykcji
  if (is.null(probabilities_output) || length(probabilities_output) != nrow(X_test_data)) {
    probabilities_output <- rep(0.5, nrow(X_test_data))
  }
  probabilities_output <- as.numeric(probabilities_output)

  if (is.null(raw_predictions_numeric) || length(raw_predictions_numeric) != nrow(X_test_data)) {
    raw_predictions_numeric <- rep(0, nrow(X_test_data))
  }
  if (!all(raw_predictions_numeric %in% c(0, 1))) {
    raw_predictions_numeric <- ifelse(as.numeric(raw_predictions_numeric) > 0.5, 1, 0)
  }

  # Tworzenie wersji faktorowych dla macierzy konfuzji
  predictions_factor_cm <- factor(raw_predictions_numeric, levels = c(0, 1), labels = c("No", "Yes"))
  y_test_factor_cm <- factor(y_test_data_numeric, levels = c(0, 1), labels = c("No", "Yes"))

  cm <- confusionMatrix(predictions_factor_cm, y_test_factor_cm, positive = "Yes")

  roc_obj <- roc(response = y_test_factor_cm, predictor = probabilities_output,
                 levels = c("No", "Yes"), direction = "<", quiet = TRUE)
  auc_score <- auc(roc_obj)

  results <- list(
    model_name = model_name_str,
    accuracy = cm$overall['Accuracy'],
    sensitivity = cm$byClass['Sensitivity'],
    specificity = cm$byClass['Specificity'],
    precision = cm$byClass['Precision'],
    f1 = cm$byClass['F1'],
    auc = as.numeric(auc_score),
    confusion_matrix = cm$table,
    roc = roc_obj,
    predictions = raw_predictions_numeric,
    probabilities = probabilities_output
  )
  return(results)
}

# Trenowanie modeli z ważeniem klas (HYBRID RESAMPLING + CLASS WEIGHTING)
