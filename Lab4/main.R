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

# # Undersampling naszej daty
# undersampled_data <- downSample(bank[, -17], bank$y)
# summary(undersampled_data)

# Podział na zbiory treningowy i testowy
set.seed(123)
train_indices <- createDataPartition(bank$y, p = 0.7, list = FALSE)
train_data <- bank[train_indices,]
test_data <- bank[-train_indices,]

# EXPERIMENT 1: Różne liczby neuronów
cat("=== EKSPERYMENT 1: Różne liczby neuronów ===\n")

neurons_to_test <- c(5, 10, 15, 20, 25, 30)
results_neurons <- data.frame()

for (neurons in neurons_to_test) {
  cat(paste("Testowanie", neurons, "neuronów...\n"))

  model <- nnet(y ~ ., data = train_data,
                size = neurons,
                decay = 0.01,
                maxit = 300,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$y)

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
cat("\n=== EKSPERYMENT 2: Różne wartości decay ===\n")

decay_values <- c(0, 0.001, 0.01, 0.1, 0.5, 1.0)
results_decay <- data.frame()

for (decay_val in decay_values) {
  cat(paste("Testowanie decay =", decay_val, "...\n"))

  model <- nnet(y ~ ., data = train_data,
                size = 20,  # Ustal na 20 neuronów
                decay = decay_val,
                maxit = 300,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$y)

  results_decay <- rbind(results_decay, data.frame(
    Decay = decay_val,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  ))
}

print("Wyniki dla różnych wartości decay:")
print(results_decay)

# Wykres
plot(results_decay$Decay, results_decay$Accuracy,
     type = "o", col = "red", pch = 16,
     xlab = "Decay", ylab = "Dokładność",
     main = "Wpływ regularyzacji na dokładność")

# EXPERIMENT 3: Różne liczby iteracji
cat("\n=== EKSPERYMENT 3: Różne liczby iteracji ===\n")

maxit_values <- c(50, 100, 200, 300, 500, 1000)
results_maxit <- data.frame()

for (maxit_val in maxit_values) {
  cat(paste("Testowanie maxit =", maxit_val, "...\n"))

  model <- nnet(y ~ ., data = train_data,
                size = 20,
                decay = 0.01,
                maxit = maxit_val,
                MaxNWts = 10000,
                trace = FALSE)

  pred <- predict(model, test_data, type = "class")
  cm <- confusionMatrix(factor(pred), test_data$y)

  results_maxit <- rbind(results_maxit, data.frame(
    MaxIt = maxit_val,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity']
  ))
}

print("Wyniki dla różnych liczb iteracji:")
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
final_model <- nnet(y ~ ., data = train_data,
                    size = best_neurons,
                    decay = best_decay,
                    maxit = best_maxit,
                    MaxNWts = 10000,
                    trace = FALSE)

# Wizualizacja
plotnet(final_model)

# Finalny test
final_pred <- predict(final_model, test_data, type = "class")
final_cm <- confusionMatrix(factor(final_pred), test_data$y)

print("WYNIKI FINALNEGO MODELU:")
print(final_cm)

# Analiza ważności zmiennych
neuralweights(final_model)