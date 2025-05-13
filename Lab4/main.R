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

# Undersampling naszej daty
undersampled_data <- downSample(bank[, -17], bank$y)
summary(undersampled_data)
table(undersampled_data$Class)


# Podział na zbiory treningowy i testowy
set.seed(123)
train_indices <- createDataPartition(undersampled_data$Class, p = 0.7, list = FALSE)
train_data <- undersampled_data[train_indices,]
test_data <- undersampled_data[-train_indices,]

# EXPERIMENT 1: Różne liczby neuronów
cat("=== EKSPERYMENT 1: Rożne liczby neuronow ===\n")

neurons_to_test <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
results_neurons <- data.frame()

for (neurons in neurons_to_test) {
  cat(paste("Testowanie", neurons, "neuronow...\n"))

  model <- nnet(Class ~ ., data = train_data,
                size = neurons,
                decay = 0.01,
                maxit = 300,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$Class)

  results_neurons <- rbind(results_neurons, data.frame(
    Neurons = neurons,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  ))
}

print("Wyniki dla roznych liczb neuronow:")
print(results_neurons)

# Wykres wyników
plot(results_neurons$Neurons, results_neurons$Accuracy,
     type = "o", col = "blue", pch = 16,
     xlab = "Liczba neuronow", ylab = "Dokładność",
     main = "Wpływ liczby neuronów na dokładność")

# EXPERIMENT 2: Różne wartości decay (regularyzacja)
cat("\n=== EKSPERYMENT 2: Rozne wartości decay ===\n")

decay_values <- c(0, 0.0001, 0.001, 0.01, 0.1, 0.5, 1.0, 2.0)
results_decay <- data.frame()

for (decay_val in decay_values) {
  cat(paste("Testowanie decay =", decay_val, "...\n"))

  model <- nnet(Class ~ ., data = train_data,
                size = 20,  # Ustal na 20 neuronów
                decay = decay_val,
                maxit = 300,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$Class)

  results_decay <- rbind(results_decay, data.frame(
    Decay = decay_val,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  ))
}

print("Wyniki dla roznych wartosci decay:")
print(results_decay)

# Wykres
plot(results_decay$Decay, results_decay$Accuracy,
     type = "o", col = "red", pch = 16,
     xlab = "Decay", ylab = "Dokładność",
     main = "Wpływ regularyzacji na dokladnosc")

# EXPERIMENT 3: Różne liczby iteracji
cat("\n=== EKSPERYMENT 3: Rozne liczby iteracji ===\n")

maxit_values <- c(50, 100, 200, 300, 500, 1000, 2000, 5000, 10000)
results_maxit <- data.frame()

for (maxit_val in maxit_values) {
  cat(paste("Testowanie maxit =", maxit_val, "...\n"))

  model <- nnet(Class ~ ., data = train_data,
                size = 20,
                decay = 0.01,
                maxit = maxit_val,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$Class)

  results_maxit <- rbind(results_maxit, data.frame(
    MaxIt = maxit_val,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  ))
}

print("Wyniki dla roznych liczb iteracji:")
print(results_maxit)

# NAJLEPSZY MODEL
cat("\n=== NAJLEPSZY MODEL ===\n")

# Znajdź najlepsze parametry z poprzednich eksperymentów
best_neurons <- results_neurons[which.max(results_neurons$Accuracy), "Neurons"]
best_decay <- results_decay[which.max(results_decay$Accuracy), "Decay"]
best_maxit <- results_maxit[which.max(results_maxit$Accuracy), "MaxIt"]

cat(paste("Najlepsze parametry: neurons =", best_neurons,
          "decay =", best_decay, "maxit =", best_maxit, "\n"))

# Buduj finalny model
final_model <- nnet(Class ~ ., data = train_data,
                    size = best_neurons,
                    decay = best_decay,
                    maxit = best_maxit,
                    MaxNWts = 10000,
                    trace = FALSE)

# Wizualizacja
plotnet(final_model)

# Finalny test
final_pred <- predict(final_model, test_data, type = "class")
final_cm <- confusionMatrix(factor(final_pred), test_data$Class)

print("WYNIKI FINALNEGO MODELU:")
print(final_cm)

# Analiza ważności zmiennych
neuralweights(final_model)


##### EXPERIMENTAL
# EXPERIMENT 4: GRID SEARCH - wszystkie kombinacje
cat("\n=== EKSPERYMENT 4: GRID SEARCH - wszystkie kombinacje ===\n")

# Definicja zakresów parametrów (ograniczone, żeby nie trwało wieki)
neurons_grid <- c(5, 10, 15, 20, 25, 30)
decay_grid <- c(0, 0.001, 0.01, 0.1, 0.5)
maxit_grid <- c(100, 300, 500, 1000, 2000)

# Stwórz wszystkie kombinacje
grid_combinations <- expand.grid(
  Neurons = neurons_grid,
  Decay = decay_grid,
  MaxIt = maxit_grid
)

# Informacje o eksperymencie
cat("Liczba kombinacji do przetestowania:", nrow(grid_combinations), "\n")
cat("Przewidywany czas: około", round(nrow(grid_combinations) * 0.5 / 60, 1), "minut\n")
cat("Rozpoczynanie grid search...\n\n")

# DataFrame dla wyników
grid_results <- data.frame()

# Progress tracker
start_time <- Sys.time()

# Główna pętla grid search
for (i in seq_len(nrow(grid_combinations))) {
  # Pobierz parametry dla tej kombinacji
  neurons <- grid_combinations$Neurons[i]
  decay <- grid_combinations$Decay[i]
  maxit <- grid_combinations$MaxIt[i]

  # Progress output
  if (i %% 10 == 0 || i == 1) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    remaining <- (elapsed / i) * (nrow(grid_combinations) - i)
    cat(sprintf("Progress: %d/%d (%.1f%%) - ETA: %.1f min\n",
                i, nrow(grid_combinations),
                (i/nrow(grid_combinations))*100,
                remaining))
  }

  # Train model z aktualną kombinacją
  tryCatch({
    model <- nnet(Class ~ ., data = train_data,
                  size = neurons,
                  decay = decay,
                  maxit = maxit,
                  MaxNWts = 10000,
                  trace = FALSE)

    # Predykcje
    pred <- predict(model, test_data, type = "class")
    cm <- confusionMatrix(factor(pred), test_data$Class)

    # Zapisz wyniki
    grid_results <- rbind(grid_results, data.frame(
      Combination = i,
      Neurons = neurons,
      Decay = decay,
      MaxIt = maxit,
      Accuracy = cm$overall['Accuracy'],
      Sensitivity = cm$byClass['Sensitivity'],
      Specificity = cm$byClass['Specificity'],
      Kappa = cm$overall['Kappa'],
      Balanced_Accuracy = (cm$byClass['Sensitivity'] + cm$byClass['Specificity']) / 2
    ))

  }, error = function(e) {
    # Jeśli model się nie uda, zapisz NA
    grid_results <<- rbind(grid_results, data.frame(
      Combination = i,
      Neurons = neurons,
      Decay = decay,
      MaxIt = maxit,
      Accuracy = NA,
      Sensitivity = NA,
      Specificity = NA,
      Kappa = NA,
      Balanced_Accuracy = NA
    ))
    cat("Error in combination", i, ":", e$message, "\n")
  })
}

# Wyniki Grid Search
cat("\n=== WYNIKI GRID SEARCH ===\n")

# Usuń rekory z błędami
grid_results_clean <- grid_results[!is.na(grid_results$Accuracy), ]

# Znajdź najlepsze kombinacje dla różnych metryk
best_accuracy <- grid_results_clean[which.max(grid_results_clean$Accuracy), ]
best_specificity <- grid_results_clean[which.max(grid_results_clean$Specificity), ]
best_balanced <- grid_results_clean[which.max(grid_results_clean$Balanced_Accuracy), ]
best_kappa <- grid_results_clean[which.max(grid_results_clean$Kappa), ]

cat("NAJLEPSZE KOMBINACJE:\n\n")

cat("Dla Accuracy (", round(best_accuracy$Accuracy, 4), "):\n")
cat("  Neurons:", best_accuracy$Neurons, ", Decay:", best_accuracy$Decay,
    ", MaxIt:", best_accuracy$MaxIt, "\n\n")

cat("Dla Specificity (", round(best_specificity$Specificity, 4), "):\n")
cat("  Neurons:", best_specificity$Neurons, ", Decay:", best_specificity$Decay,
    ", MaxIt:", best_specificity$MaxIt, "\n\n")

cat("Dla Balanced Accuracy (", round(best_balanced$Balanced_Accuracy, 4), "):\n")
cat("  Neurons:", best_balanced$Neurons, ", Decay:", best_balanced$Decay,
    ", MaxIt:", best_balanced$MaxIt, "\n\n")

cat("Dla Kappa (", round(best_kappa$Kappa, 4), "):\n")
cat("  Neurons:", best_kappa$Neurons, ", Decay:", best_kappa$Decay,
    ", MaxIt:", best_kappa$MaxIt, "\n\n")

# Top 10 kombinacji
cat("TOP 10 KOMBINACJI (według Balanced Accuracy):\n")
grid_results_sorted <- grid_results_clean[order(-grid_results_clean$Balanced_Accuracy), ]
print(head(grid_results_sorted[, c("Neurons", "Decay", "MaxIt", "Accuracy",
                                   "Sensitivity", "Specificity", "Balanced_Accuracy")], 10))

# FINALNY MODEL z najlepszą kombinacją
cat("\n=== FINALNY MODEL GRID SEARCH ===\n")

# Wybierz najlepszą kombinację (według balanced accuracy)
final_neurons <- best_balanced$Neurons
final_decay <- best_balanced$Decay
final_maxit <- best_balanced$MaxIt

cat("Najlepsza kombinacja:\n")
cat("  Neurons:", final_neurons, ", Decay:", final_decay, ", MaxIt:", final_maxit, "\n\n")

# Train finalny model
final_model_grid <- nnet(Class ~ ., data = train_data,
                         size = final_neurons,
                         decay = final_decay,
                         maxit = final_maxit,
                         MaxNWts = 10000,
                         trace = FALSE)

# Test finalnego modelu
final_pred_grid <- predict(final_model_grid, test_data, type = "class")
final_cm_grid <- confusionMatrix(factor(final_pred_grid), test_data$Class)

print("WYNIKI FINALNEGO MODELU GRID SEARCH:")
print(final_cm_grid)

# Wizualizacja najlepszego modelu
plotnet(final_model_grid)

# Analiza ważności zmiennych dla najlepszego modelu
cat("=== ANALIZA WAŻNOŚCI ZMIENNYCH ===\n")
neuralweights(final_model_grid)

# Wizualizacja wyników grid search
# Heatmapa dla kombinacji decay i neurons (średnio po maxit)
library(reshape2)
library(ggplot2)

# Agreguj wyniki według neurons i decay
heatmap_data <- aggregate(Balanced_Accuracy ~ Neurons + Decay,
                          data = grid_results_clean,
                          FUN = mean, na.rm = TRUE)

# Przekształć do formatu macierzy
heatmap_matrix <- acast(heatmap_data, Neurons ~ Decay, value.var = "Balanced_Accuracy")

# Plot heatmap
par(mar = c(5, 5, 4, 2))
image(seq_len(ncol(heatmap_matrix)), seq_len(nrow(heatmap_matrix)),
      t(heatmap_matrix),
      xlab = "Decay", ylab = "Neurons",
      main = "Grid Search Heatmap - Balanced Accuracy",
      col = heat.colors(20),
      xaxt = "n", yaxt = "n")

# Dodaj osie
axis(1, at = seq_len(ncol(heatmap_matrix)), labels = colnames(heatmap_matrix))
axis(2, at = seq_len(nrow(heatmap_matrix)), labels = rownames(heatmap_matrix))

# Dodaj wartości na heatmapie
for(i in seq_len(nrow(heatmap_matrix))) {
  for(j in seq_len(ncol(heatmap_matrix))) {
    if(!is.na(heatmap_matrix[i,j])) {
      text(j, i, round(heatmap_matrix[i,j], 3), cex = 0.8)
    }
  }
}

cat("\nGrid Search zakończony! Czas total:",
    round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1), "minut\n")