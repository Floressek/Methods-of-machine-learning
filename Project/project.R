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
library(ROSE)  # Dla SMOTE

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
cat("12. Diabetic: Cukrzyca (Tak/Nie)\n")
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

# Konwersja stringow NA na R NA czyli usunięcie spacji i konwersja na numeryczne
cat("Krok 1: Konwersja stringowych reprezentacji NA...\n")
na_strings_to_check <- c("NA", "N/A", "na", "n/a", "", " ", "NULL", "Null", "null", "Missing", "?", ".", "Unknown", "Not Specified")
for (col_name in names(data_clean)) {
  if (is.character(data_clean[[col_name]])) {
    na_before <- sum(is.na(data_clean[[col_name]]))
    data_clean[[col_name]][trimws(data_clean[[col_name]]) %in% na_strings_to_check] <- NA # Konwersja stringów NA na R NA
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

numerical_features <- c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")
numerical_features <- intersect(numerical_features, names(data_clean))

# Utworzenie listy cech kategorycznych
all_columns <- names(data_clean)
categorical_features <- setdiff(all_columns, c(numerical_features, "HeartDisease"))

cat("\n--- KONWERSJA KOLUMN NA ODPOWIEDNIE TYPY FAKTORÓW ---\n")
# Konwersja zmiennych binarnych na faktory
binary_yes_no_cols <- c("Smoking", "AlcoholDrinking", "Stroke", "DiffWalking",
                        "Diabetic", "PhysicalActivity", "Asthma", "KidneyDisease", "SkinCancer")
binary_yes_no_cols <- intersect(binary_yes_no_cols, names(data_clean))

for (col in binary_yes_no_cols) {
  unique_values <- unique(data_clean[[col]])
  expected_values <- c("No", "Yes")
  if (!all(unique_values %in% expected_values)) {
    # Sprawdzenie, czy kolumna zawiera tylko oczekiwane wartości
    warning(paste("Kolumna '", col, "' zawiera wartości inne niż 'No', 'Yes': ",
                  paste(setdiff(unique_values, expected_values), collapse = ", ")))
  }
  data_clean[[col]] <- factor(data_clean[[col]], levels = c("Yes", "No"))
  cat("Przekonwertowano '", col, "' na faktor.\n")
}


# Plec na faktor
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

# Konwersja ogolnego stany zdrowia na faktor
gen_health_levels_ordered <- c("Poor", "Fair", "Good", "Very good", "Excellent")
if ("GenHealth" %in% names(data_clean)) {
  data_clean$GenHealth <- factor(data_clean$GenHealth, levels = gen_health_levels_ordered, ordered = TRUE)
  cat("Przekonwertowano 'GenHealth' na faktor.\n")
}

# Konwersja zmiennej docelowej HeartDisease na faktor
if ("HeartDisease" %in% names(data_clean)) {
  data_clean$HeartDisease <- factor(data_clean$HeartDisease, levels = c("Yes", "No"))
  cat("Przekonwertowano 'HeartDisease' na faktor.\n")
} else { # In case of data corruption or missing column
  stop("Kolumna 'HeartDisease' nie została znaleziona w danych.")
}

# Finalna kontrola NA po konwersji danych na faktor
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
glimpse(data_clean)
str(data_clean, list.len = ncol(data_clean))

# ===========================
# 3. EKSPLORACYJNA ANALIZA DANYCH
# ===========================

if (nrow(data_clean) > 0) {
  # Rozklad zmiennej docelowej
  target_plot <- ggplot(data_clean, aes(x = HeartDisease, fill = HeartDisease)) +
    geom_bar(alpha = 0.8) +
    geom_text(stat = 'count', aes(
      label = paste0(
        after_stat(count),
        '\n',
        scales::percent(
          after_stat(count) / sum(after_stat(count)
          )
        )
      )
    ),
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

  # Wykresy rozkladow dla cech numerycznych
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
    if (length(plot_list_num) > 0) grid.arrange(grobs = plot_list_num, ncol = 2) # Wyswietlenie wykresów
  }

  # Analiza cech numerycznych
  if (length(categorical_features) > 0) {
    plot_list_num <- list()
    for (i in seq_along(categorical_features)) {
      features <- categorical_features[i]
      p <- ggplot(data_clean, aes_string(x = feature, fill = "HeartDisease")) +
        geom_bar(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("No" = "#3FFEBA", "Yes" = "#FC05FB")) +
        labs(title = paste("Rozklad", feature), x = feature, y = "Liczba") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      plot_list_cat[[i]] <- p
    }
    if (length(plot_list_cat) > 0) grid.arrange(grobs = plot_list_cat, ncol = min(2, length(plot_list_cat)))
  }
}

# ===========================
# 4. PRZYGOTOWANIE MODELI
# ===========================


