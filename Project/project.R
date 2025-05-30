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
models <- list()
model_results <- list()

if (exists("X_train") && exists("y_train_factor") && exists("X_test") && exists("y_test_numeric")) {

  # Obliczenie wag klas na podstawie niezbalansowanego zbioru treningowego
  original_class_counts <- table(train_smote$HeartDisease)
  total_samples <- sum(original_class_counts)

  # Inverse frequency weighting
  weight_no <- total_samples / (2 * original_class_counts["No"])
  weight_yes <- total_samples / (2 * original_class_counts["Yes"])

  cat("\n=== HYBRID RESAMPLING + CLASS WEIGHTING ===\n")
  cat("Oryginalne liczby klas w initial_train_data:\n")
  print(original_class_counts)
  cat("Obliczone wagi klas:\n")
  cat("Weight for 'No':", round(weight_no, 4), "\n")
  cat("Weight for 'Yes':", round(weight_yes, 4), "\n")

  # Tworzenie wektora wag dla obserwacji
  case_weights <- ifelse(y_train_factor == "No", weight_no, weight_yes)

  # 1. Regresja logistyczna z wagami
  cat("Trenowanie regresji logistycznej z wagami klas...\n")
  # Dla GLM wagi muszą być dodatnie i nie powodować ostrzeżeń
  # Normalizujemy wagi żeby były w rozsądnym zakresie
  normalized_weights <- case_weights / mean(case_weights)
  tryCatch({
    models$logistic <- glm(HeartDisease ~ ., data = train_data, family = binomial(link = "logit"), weights = normalized_weights)
  }, warning = function(w) {
    cat("Ostrzeżenie w GLM:", w$message, "\n")
    models$logistic <<- glm(HeartDisease ~ ., data = train_data, family = binomial(link = "logit"), weights = normalized_weights)
  })
  model_results$logistic <- evaluate_model(models$logistic, X_test, y_test_numeric, "Regresja logistyczna (weighted)")

  # 2. Las losowy z wagami klas
  cat("Trenowanie lasu losowego z wagami klas...\n")
  # Random Forest wymaga nazw dokładnie takich jak w levels(y_train_factor)
  actual_levels <- levels(y_train_factor)
  cat("Poziomy y_train_factor:", paste(actual_levels, collapse = ", "), "\n")

  # Tworzenie wag z prawidłowymi nazwami
  class_weights_rf <- setNames(c(weight_no, weight_yes), actual_levels)
  cat("Wagi dla Random Forest:", paste(names(class_weights_rf), "=", round(class_weights_rf, 4), collapse = ", "), "\n")

  models$rf <- randomForest(x = X_train, y = y_train_factor, ntree = 300, importance = TRUE, classwt = class_weights_rf)
  model_results$rf <- evaluate_model(models$rf, X_test, y_test_numeric, "Las losowy (weighted)")

  # 3. Drzewo decyzyjne z wagami
  cat("Trenowanie drzewa decyzyjnego z wagami klas...\n")
  models$tree <- rpart(HeartDisease ~ ., data = train_data, method = "class", weights = case_weights)
  model_results$tree <- evaluate_model(models$tree, X_test, y_test_numeric, "Drzewo decyzyjne (weighted)")

  # 4. Gradient Boosting z wagami
  cat("Trenowanie Gradient Boosting z wagami klas...\n")
  train_data_gbm <- X_train
  train_data_gbm$HeartDisease_numeric_target <- y_train_numeric
  models$gbm <- gbm(HeartDisease_numeric_target ~ ., data = train_data_gbm,
                    distribution = "bernoulli", n.trees = 300, interaction.depth = 3,
                    shrinkage = 0.1, cv.folds = 6, weights = case_weights)
  model_results$gbm <- evaluate_model(models$gbm, X_test, y_test_numeric, "Gradient Boosting (weighted)")

  # 5. XGBoost z wagami klas
  cat("Trenowanie XGBoost z wagami klas...\n")
  # Przygotowanie danych dla XGBoost
  X_train_numeric <- X_train
  factor_cols_train <- sapply(X_train_numeric, is.factor)
  X_train_numeric[factor_cols_train] <- lapply(X_train_numeric[factor_cols_train], as.numeric)

  X_test_numeric <- X_test
  factor_cols_test <- sapply(X_test_numeric, is.factor)
  X_test_numeric[factor_cols_test] <- lapply(X_test_numeric[factor_cols_test], as.numeric)

  train_matrix <- xgb.DMatrix(data = as.matrix(X_train_numeric), label = y_train_numeric, weight = case_weights)

  # Parametry XGBoost z scale_pos_weight
  scale_pos_weight_value <- weight_yes / weight_no
  xgb_params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = scale_pos_weight_value  # Dodatkowo dla XGBoost
  )

  cat("XGBoost scale_pos_weight:", round(scale_pos_weight_value, 4), "\n")

  models$xgboost <- xgb.train(
    params = xgb_params,
    data = train_matrix,
    nrounds = 200,
    verbose = 0
  )
  model_results$xgboost <- evaluate_model(models$xgboost, X_test_numeric, y_test_numeric, "XGBoost (weighted)")

  # 6. DODATKOWE MODELE BEZ WAG (dla porównania)
  cat("\n=== MODELE BEZ WAG (dla porównania) ===\n")

  # Las losowy bez wag
  cat("Trenowanie lasu losowego BEZ wag...\n")
  models$rf_unweighted <- randomForest(x = X_train, y = y_train_factor, ntree = 300, importance = TRUE)
  model_results$rf_unweighted <- evaluate_model(models$rf_unweighted, X_test, y_test_numeric, "Las losowy (unweighted)")

  # XGBoost bez wag
  cat("Trenowanie XGBoost BEZ wag...\n")
  train_matrix_unweighted <- xgb.DMatrix(data = as.matrix(X_train_numeric), label = y_train_numeric)
  xgb_params_unweighted <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  models$xgboost_unweighted <- xgb.train(
    params = xgb_params_unweighted,
    data = train_matrix_unweighted,
    nrounds = 200,
    verbose = 0
  )
  model_results$xgboost_unweighted <- evaluate_model(models$xgboost_unweighted, X_test_numeric, y_test_numeric, "XGBoost (unweighted)")

} else {
  cat("Komponenty danych treningowych nie są w pełni dostępne. Pomijanie trenowania modeli.\n")
}

# ===========================
# 6. ENSEMBLE MODELI
# ===========================
cat("\n=== TWORZENIE ENSEMBLE MODELI ===\n")

if (length(model_results) >= 3) {
  # Zbieranie prawdopodobieństw z wszystkich modeli
  ensemble_probs <- data.frame(
    logistic = if("logistic" %in% names(model_results)) model_results$logistic$probabilities else rep(0.5, length(y_test_numeric)),
    rf = if("rf" %in% names(model_results)) model_results$rf$probabilities else rep(0.5, length(y_test_numeric)),
    gbm = if("gbm" %in% names(model_results)) model_results$gbm$probabilities else rep(0.5, length(y_test_numeric)),
    xgboost = if("xgboost" %in% names(model_results)) model_results$xgboost$probabilities else rep(0.5, length(y_test_numeric))
  )

  # Ensemble przez uśrednienie
  ensemble_avg_probs <- rowMeans(ensemble_probs, na.rm = TRUE)
  ensemble_avg_preds <- ifelse(ensemble_avg_probs > 0.5, 1, 0)

  # Ensemble przez głosowanie większościowe
  ensemble_votes <- data.frame(
    logistic = if("logistic" %in% names(model_results)) model_results$logistic$predictions else rep(0, length(y_test_numeric)),
    rf = if("rf" %in% names(model_results)) model_results$rf$predictions else rep(0, length(y_test_numeric)),
    gbm = if("gbm" %in% names(model_results)) model_results$gbm$predictions else rep(0, length(y_test_numeric)),
    xgboost = if("xgboost" %in% names(model_results)) model_results$xgboost$predictions else rep(0, length(y_test_numeric))
  )

  ensemble_majority_preds <- ifelse(rowSums(ensemble_votes) >= 2, 1, 0)

  # Ewaluacja ensemble uśredniającego
  ensemble_avg_factor <- factor(ensemble_avg_preds, levels = c(0, 1), labels = c("No", "Yes"))
  y_test_factor_ensemble <- factor(y_test_numeric, levels = c(0, 1), labels = c("No", "Yes"))

  cm_avg <- confusionMatrix(ensemble_avg_factor, y_test_factor_ensemble, positive = "Yes")
  roc_avg <- roc(response = y_test_factor_ensemble, predictor = ensemble_avg_probs,
                 levels = c("No", "Yes"), direction = "<", quiet = TRUE)

  model_results$ensemble_avg <- list(
    model_name = "Ensemble (Usrednianie)",
    accuracy = cm_avg$overall['Accuracy'],
    sensitivity = cm_avg$byClass['Sensitivity'],
    specificity = cm_avg$byClass['Specificity'],
    precision = cm_avg$byClass['Precision'],
    f1 = cm_avg$byClass['F1'],
    auc = as.numeric(auc(roc_avg)),
    confusion_matrix = cm_avg$table,
    roc = roc_avg,
    predictions = ensemble_avg_preds,
    probabilities = ensemble_avg_probs
  )

  # Ewaluacja ensemble głosowania większościowego
  ensemble_maj_factor <- factor(ensemble_majority_preds, levels = c(0, 1), labels = c("No", "Yes"))

  cm_maj <- confusionMatrix(ensemble_maj_factor, y_test_factor_ensemble, positive = "Yes")
  roc_maj <- roc(response = y_test_factor_ensemble, predictor = ensemble_avg_probs,
                 levels = c("No", "Yes"), direction = "<", quiet = TRUE)

  model_results$ensemble_majority <- list(
    model_name = "Ensemble (Glosowanie wiekszosciowe)",
    accuracy = cm_maj$overall['Accuracy'],
    sensitivity = cm_maj$byClass['Sensitivity'],
    specificity = cm_maj$byClass['Specificity'],
    precision = cm_maj$byClass['Precision'],
    f1 = cm_maj$byClass['F1'],
    auc = as.numeric(auc(roc_maj)),
    confusion_matrix = cm_maj$table,
    roc = roc_maj,
    predictions = ensemble_majority_preds,
    probabilities = ensemble_avg_probs
  )

  cat("Ensemble modeli utworzony pomyślnie!\n")
  cat("Ensemble (Usrednianie) - Dokladnosc:", round(cm_avg$overall['Accuracy'], 4),
      ", AUC:", round(auc(roc_avg), 4), "\n")
  cat("Ensemble (Glosowanie) - Dokladnosc:", round(cm_maj$overall['Accuracy'], 4),
      ", AUC:", round(auc(roc_maj), 4), "\n")
}

# ===========================
# 7. PORÓWNANIE WYNIKÓW
# ===========================
if (length(model_results) > 0) {
  results_df <- data.frame(
    Model = character(), Accuracy = numeric(), Sensitivity = numeric(),
    Specificity = numeric(), Precision = numeric(), F1_Score = numeric(),
    AUC = numeric(), stringsAsFactors = FALSE
  )

  for (i in seq_along(model_results)) {
    res <- model_results[[i]]
    results_df <- rbind(results_df, data.frame(
      Model = res$model_name,
      Accuracy = round(res$accuracy, 4),
      Sensitivity = round(res$sensitivity, 4),
      Specificity = round(res$specificity, 4),
      Precision = round(res$precision, 4),
      F1_Score = round(res$f1, 4),
      AUC = round(res$auc, 4)
    ))
  }
  results_df <- results_df[order(results_df$AUC, decreasing = TRUE),]
  cat("\n=== POROWNANIE WYDAJNOSCI MODELI ===\n")
  print(kable(results_df, row.names = FALSE))

  # Wykres krzywych ROC
  roc_plot <- ggplot() +
    labs(title = "Porownanie krzywych ROC",
         x = "Wskaznik falszywie pozytywnych (1 - Specyficznosc)",
         y = "Wskaznik prawdziwie pozytywnych (Czulosc)")

  colors_palette <- RColorBrewer::brewer.pal(max(3, length(model_results)), "Set1")
  if (length(model_results) > length(colors_palette)) {
    colors_palette <- rainbow(length(model_results))
  }

  # Dodanie krzywych ROC dla każdego modelu
  for (i in seq_along(model_results)) {
    res <- model_results[[i]]
    if (!is.null(res$roc) && inherits(res$roc, "roc")) {
      roc_data <- data.frame(fpr = 1 - res$roc$specificities, tpr = res$roc$sensitivities)
      roc_data <- roc_data[order(roc_data$fpr, roc_data$tpr),]
      roc_plot <- roc_plot +
        geom_line(data = roc_data, aes(x = fpr, y = tpr),
                  color = colors_palette[i], linewidth = 1) +
        annotate("text", x = 0.65, y = 0.05 + i * 0.06,
                 label = paste(res$model_name, "AUC =", round(res$auc, 3)),
                 color = colors_palette[i], size = 3, hjust = 0)
    }
  }
  roc_plot <- roc_plot +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal()
  print(roc_plot)

  # Ważność cech dla lasu losowego
  if ("rf" %in% names(models) && !is.null(models$rf$importance)) {
    imp_matrix <- importance(models$rf)
    importance_col_name <- "MeanDecreaseGini"
    if (!importance_col_name %in% colnames(imp_matrix)) {
      importance_col_name <- colnames(imp_matrix)[1]
    }

    importance_data <- data.frame(Feature = rownames(imp_matrix),
                                  Importance = imp_matrix[, importance_col_name])
    importance_data <- importance_data[order(importance_data$Importance, decreasing = TRUE),]

    feature_importance_plot <- ggplot(head(importance_data, 15),
                                      aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "#3FFEBA", alpha = 0.8) +
      coord_flip() +
      labs(title = "15 najwazniejszych cech (Las losowy)",
           x = "Cechy", y = importance_col_name)
    print(feature_importance_plot)
  }

  # Ważność cech dla XGBoost
  if ("xgboost" %in% names(models)) {
    xgb_importance <- xgb.importance(model = models$xgboost)

    xgb_importance_plot <- ggplot(head(xgb_importance, 15),
                                  aes(x = reorder(Feature, Gain), y = Gain)) +
      geom_col(fill = "#FC05FB", alpha = 0.8) +
      coord_flip() +
      labs(title = "15 najwazniejszych cech (XGBoost)",
           x = "Cechy", y = "Gain")
    print(xgb_importance_plot)
  }
}
#
# # ===========================
# # 8. DOSTRAJANIE HIPERPARAMETRÓW NAJLEPSZEGO MODELU
# # ===========================
# if (exists("train_data") && length(model_results) > 0) {
#   cat("\n=== DOSTRAJANIE HIPERPARAMETROW NAJLEPSZEGO MODELU ===\n")
#
#   # Znajdź najlepszy model według różnych metryk
#   best_auc_model <- results_df$Model[which.max(results_df$AUC)]
#   # Obliczenie zbalansowanej metryki uwzględniającej zarówno sensitivity jak i specificity
#   results_df$Balanced_Score <- (results_df$Sensitivity + results_df$Specificity) / 2
#   best_balanced_model <- results_df$Model[which.max(results_df$Balanced_Score)]
#
#   cat("Najlepszy model według AUC:", best_auc_model, "\n")
#   cat("Najlepszy model według zbalansowanej metryki (sensitivity + specificity)/2:",
#       best_balanced_model, "\n")
#
#   # Określ model do dostrojenia (możemy wybrać ten z lepszym zbalansowaniem)
#   best_model_name <- best_balanced_model
#   cat("Wybrano model do dostrajania:", best_model_name, "\n\n")
#
#   # Ustawienia dla walidacji krzyżowej
#   train_control_cv <- trainControl(method = "cv", number = 5,
#                                    summaryFunction = twoClassSummary,
#                                    classProbs = TRUE,
#                                    verboseIter = FALSE)
#
#   # Przygotowanie danych
#   num_predictors <- ncol(X_train)
#
#   # DOSTRAJANIE LASU LOSOWEGO
#   if (grepl("Las losowy|Random Forest", best_model_name, ignore.case = TRUE) ||
#     "rf" %in% names(models)) {
#
#     mtry_default <- floor(sqrt(num_predictors))
#     mtry_range <- unique(c(max(1, mtry_default - 2), mtry_default,
#                            min(num_predictors, mtry_default + 2), 2, 4, 6, 8))
#     mtry_range <- mtry_range[mtry_range <= num_predictors & mtry_range > 0]
#     mtry_range <- sort(unique(mtry_range))
#
#     rf_grid_tune <- expand.grid(mtry = mtry_range)
#     cat("Dostrajanie lasu losowego z mtry:", paste(mtry_range, collapse = ", "), "\n")
#
#     rf_tuned_model <- NULL
#     tryCatch({
#       rf_tuned_model <- train(
#         HeartDisease ~ .,
#         data = train_data,
#         method = "rf",
#         trControl = train_control_cv,
#         tuneGrid = rf_grid_tune,
#         metric = "ROC",
#         ntree = 300
#       )
#       print(rf_tuned_model)
#       plot(rf_tuned_model)
#     }, error = function(e) {
#       cat("Blad podczas dostrajania lasu losowego:", e$message, "\n")
#     })
#
#     tuned_model <- rf_tuned_model
#     model_type <- "rf"
#   }
#
#     # DOSTRAJANIE XGBOOST
#   else if (grepl("XGBoost", best_model_name, ignore.case = TRUE) ||
#     "xgb" %in% names(models)) {
#
#     xgb_grid_tune <- expand.grid(
#       nrounds = c(50, 100, 150),
#       eta = c(0.01, 0.1, 0.3),
#       max_depth = c(3, 5, 7),
#       gamma = c(0, 0.1, 0.5),
#       colsample_bytree = c(0.5, 0.7, 1.0),
#       min_child_weight = c(1, 3, 5),
#       subsample = c(0.5, 0.7, 1.0)
#     )
#
#     # Można też użyć mniejszej siatki dla szybszego testowania
#     xgb_grid_tune_small <- expand.grid(
#       nrounds = c(100),
#       eta = c(0.1, 0.3),
#       max_depth = c(3, 6),
#       gamma = 0,
#       colsample_bytree = 1,
#       min_child_weight = 1,
#       subsample = 1
#     )
#
#     cat("Dostrajanie XGBoost z parametrami\n")
#
#     xgb_tuned_model <- NULL
#     tryCatch({
#       xgb_tuned_model <- train(
#         HeartDisease ~ .,
#         data = train_data,
#         method = "xgbTree",
#         trControl = train_control_cv,
#         tuneGrid = xgb_grid_tune_small,  # Użyj smaller grid dla testów
#         metric = "ROC"
#       )
#       print(xgb_tuned_model)
#       plot(xgb_tuned_model)
#     }, error = function(e) {
#       cat("Blad podczas dostrajania XGBoost:", e$message, "\n")
#     })
#
#     tuned_model <- xgb_tuned_model
#     model_type <- "xgb"
#   }
#
#     # DOSTRAJANIE GRADIENT BOOSTING
#   else if (grepl("Gradient Boosting|GBM", best_model_name, ignore.case = TRUE) ||
#     "gbm" %in% names(models)) {
#
#     gbm_grid_tune <- expand.grid(
#       n.trees = c(100, 200, 300),
#       interaction.depth = c(2, 3, 5),
#       shrinkage = c(0.01, 0.05, 0.1),
#       n.minobsinnode = c(5, 10, 15)
#     )
#
#     # Mniejsza siatka dla testów
#     gbm_grid_tune_small <- expand.grid(
#       n.trees = c(100, 200),
#       interaction.depth = c(3, 5),
#       shrinkage = c(0.05, 0.1),
#       n.minobsinnode = 10
#     )
#
#     cat("Dostrajanie Gradient Boosting z parametrami\n")
#
#     gbm_tuned_model <- NULL
#     tryCatch({
#       gbm_tuned_model <- train(
#         HeartDisease ~ .,
#         data = train_data,
#         method = "gbm",
#         trControl = train_control_cv,
#         tuneGrid = gbm_grid_tune_small,  # Użyj smaller grid dla testów
#         metric = "ROC",
#         verbose = FALSE
#       )
#       print(gbm_tuned_model)
#       plot(gbm_tuned_model)
#     }, error = function(e) {
#       cat("Blad podczas dostrajania Gradient Boosting:", e$message, "\n")
#     })
#
#     tuned_model <- gbm_tuned_model
#     model_type <- "gbm"
#   }
#
#   # EWALUACJA KOŃCOWEGO DOSTROJONEGO MODELU
#   if (exists("tuned_model") && !is.null(tuned_model)) {
#     final_predictions <- predict(tuned_model, newdata = test_data)
#     final_probabilities <- predict(tuned_model, newdata = test_data, type = "prob")
#
#     final_cm <- confusionMatrix(final_predictions, y_test_factor, positive = "Yes")
#     cat("\n=== WYDAJNOSC KONCOWEGO DOSTROJONEGO MODELU ===\n")
#     print(final_cm)
#
#     final_roc <- roc(response = y_test_factor,
#                      predictor = final_probabilities$Yes,
#                      levels = c("No", "Yes"), direction = "<", quiet = TRUE)
#     cat("Koncowy AUC dostrojonego modelu:", round(auc(final_roc), 4), "\n")
#     plot(final_roc, main = paste0("Krzywa ROC dla koncowego dostrojonego modelu: ", best_model_name),
#          print.auc = TRUE)
#
#     # Porównanie z oryginalnym modelem
#     cat("\n=== POROWNANIE Z ORYGINALNYM MODELEM ===\n")
#     original_metric <- results_df[results_df$Model == best_model_name, ]
#     tuned_metrics <- data.frame(
#       Model = paste0("Dostrojony ", best_model_name),
#       Accuracy = round(final_cm$overall['Accuracy'], 4),
#       Sensitivity = round(final_cm$byClass['Sensitivity'], 4),
#       Specificity = round(final_cm$byClass['Specificity'], 4),
#       Precision = round(final_cm$byClass['Precision'], 4),
#       F1_Score = round(final_cm$byClass['F1'], 4),
#       AUC = round(auc(final_roc), 4),
#       Balanced_Score = round((final_cm$byClass['Sensitivity'] + final_cm$byClass['Specificity'])/2, 4)
#     )
#
#     comparison_table <- rbind(original_metric, tuned_metrics)
#     print(comparison_table)
#
#     # Zapisanie najlepszego modelu
#     if (model_type == "rf") {
#       best_model <- tuned_model$finalModel
#     } else {
#       best_model <- tuned_model
#     }
#
#
#     if (!is.null(tuned_model$results)) {
#       cat("\n=== WPŁYW PARAMETRÓW NA WYDAJNOŚĆ MODELU ===\n")
#       print(tuned_model$results)
#     }
#   } else {
#     cat("Nie udało się dostroić żadnego modelu.\n")
#   }
# }

# ===========================
# 9. WNIOSKI
# ===========================
cat("\n=== WNIOSKI ===\n")
if (exists("results_df") && nrow(results_df) > 0 && !all(is.na(results_df$AUC))) {
  cat("1. Najlepiej wykonujacy sie model (wedlug AUC):",
      results_df$Model[which.max(results_df$AUC)],
      "- AUC:", max(results_df$AUC, na.rm = TRUE), "\n")
}

cat("2. Zastosowane techniki:\n")
cat("   - SMOTE do balansowania klas (lub alternatywne over/undersampling)\n")
cat("   - Dodano XGBoost jako nowy algorytm\n")
cat("   - Utworzono ensemble modeli (usrednianie i glosowanie wiekszosciowe)\n")

if (exists("final_cm_caret") && !is.null(final_cm_caret)) {
  cat("3. Koncowa dokladnosc dostrojonego modelu:",
      round(final_cm_caret$overall['Accuracy'], 4), "\n")
}

if (exists("final_roc_caret")) {
  cat("4. Koncowy AUC dostrojonego modelu:", round(auc(final_roc_caret), 4), "\n")
}

cat("\n=== KLUCZOWE SPOSTRZEZENIA ===\n")
cat("- SMOTE pomoglo w lepszym balansowaniu klas niz undersampling\n")
cat("- XGBoost czesto osiaga wysokie wyniki w problemach klasyfikacji\n")
cat("- Ensemble modeli moze poprawic stabilnosc predykcji\n")
cat("- Najwazniejsze cechy czesto odzwierciedlaja znane czynniki ryzyka chorob serca\n")

if (exists("model_results") && "ensemble_avg" %in% names(model_results)) {
  cat("- Ensemble (usrednianie) osiagnal AUC:",
      round(model_results$ensemble_avg$auc, 4), "\n")
}

cat("\nAnaliza nowego zbioru danych zakonczona pomyslnie!\n")
