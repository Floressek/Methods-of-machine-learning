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

# Wykonanie wykresów pudełkowych dla wszystkich zmiennych numerycznych
X <- c(
  "TOTLGIVE",
  "MOV_DWEL",
  "HH_1PER",
  "HH_2PER",
  "HH_3PER",
  "HH_45PER",
  "AVE_INC",
  "DWEL_VAL",
  "CNDN_PCT",
  "ENG_PCT",
  "OWN_PCT",
  "SD_INC"
)

X_labels <- c("Total donation",
              "% of households in the region living in trailers",
              "% of single person households in the region",
              "% of two-person households in the region",
              "% of three-person households in the region",
              "% of four- to five-person households in the region",
              "Average income", "Average housing value in region",
              "% of Canadian in region",
              "% of people for whom English is their first language in the region",
              "% of households in the region with own dwelling",
              "Standard deviation for income in the region")

# Bedziemy uzywac ggplot2 dla wizualizacji wykresow
for (i in seq_along(X)) {
  cat("Tworzenie wykresu pudełkowego dla zmiennej", X[i], "\n")
  p <- ggplot(wesbrook_dataset, aes_string(x = "WESBROOK_original", y = X[i])) +
    geom_boxplot(fill = "skyblue", color = "darkblue") +
    labs(title = paste(X_labels[i], "by donation bigger than 1000$ in last year"),
         x = "Donation bigger than 1000$ in last year (WESBROOK yes/no)",
         y = X_labels[i]) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  # Zapisanie do pliku
  ggsave(paste0("data/lab-1/boxplot_", X[i], ".png"), plot = p, width = 10, height = 8, dpi = 300)
}

# Utworzenie zbioru Wesbrook2 z wybranymi zmiennymi
wesbrook2 <- wesbrook_dataset[c("TOTLGIVE", "DWEL_VAL", "AVE_INC",
                                names(wesbrook_dataset)[sapply(wesbrook_dataset, is.factor)])]

if (!"WESBROOK_original" %in% names(wesbrook2)) {
  wesbrook2$WESBROOK_original <- wesbrook_dataset$WESBROOK_original
}

# Wykresy mozaikowe dla zmiennych kategorycznych
cat("\n Tworzenie wykresów mozaikowych dla zmiennych kategorycznych:\n")
categorical_vars <- wesbrook2 %>% select_if(is.factor)
# Usuwamy zmienne, ktore moglyby sie zdoublowac dla wykresow czyli 1:1
categorical_vars <- categorical_vars[!names(categorical_vars) %in% c("WESBROOK", "WESBROOK_original")]

for (var in names(categorical_vars)) {
  cat("Tworzenie wykresu mozaikowego dla zmiennej", var, "\n")
  png(paste0("data/lab-1/mosaic_", var, "_WESBROOK.png"), width = 800, height = 600)
  mosaicplot(as.formula(paste(var, "~ WESBROOK_original")), data = wesbrook2,
             main = paste("Distribution of variable", var, "relative to WESBROOK"),
             color = TRUE,
             xlab = var,
             ylab = "WESBROOK (donation > 1000$ in a recent fiscal year)")
  dev.off()
}

# Zadanie 4: Wizualna analiza danych - histogramy
X <- c(
  "TOTLGIVE",
  "DWEL_VAL",
  "AVE_INC"
)

X_labels <- c(
  "Total donation",
  "Average housing value in region",
  "Average income"
)

for (i in seq_along(X)) {
  cat("Tworzenie histogramu dla zmiennej", X[i], "\n")
  p <- ggplot(wesbrook2) +
    geom_histogram(
      mapping = aes_string(x = X[i]),
      bins = 50,
      fill = "cyan",
      color = "black"
    ) +
    labs(title = paste("Histogram of", X_labels[i]),
         x = X_labels[i],
         y = "Frequency") +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(paste0("data/lab-1/histogram_", X[i], ".png"), plot = p, width = 10, height = 8, dpi = 300)
}

# Zadanie 5: Przygotowanie danych do analizy
cat("\n Statystyki przed przygotowaniem danych:\n")
summary(wesbrook2)
print(head(wesbrook2))

# Imputacja brakujących wartości
wesbrook2 <- wesbrook2 %>%
  mutate(AVE_INC = ifelse(is.na(AVE_INC), median(AVE_INC, na.rm = TRUE), AVE_INC))

# Imputacja zmiennej MARITAL - dodanie "U" as unknown na miejsce NA
wesbrook2$MARITAL <- as.character(wesbrook2$MARITAL) # factor -> character conversion
wesbrook2$MARITAL[is.na(wesbrook2$MARITAL)] <- "U"
wesbrook2$MARITAL <- as.factor(wesbrook2$MARITAL)

# Sprawdzenie, czy zmienne są w odpowiednim formacie
cat("\n Liczba brakujących wartości po imputacji:\n")
print(colSums(is.na(wesbrook2)))

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

# Wprowadzneie kodowania 0-1 da zmiennej MARITAL
cat("\n Unikalne wartości zmiennej MARITAL:\n")
print(table(wesbrook2$MARITAL, useNA = 'ifany')) # ifany - pokazuje tylko te zmienne, ktore sa NA

wesbrook2$MARITAL <- as.factor(wesbrook2$MARITAL)
merital_dummies <- model.matrix(~0 + MARITAL, data = wesbrook2) # ~0 - bez interceptu
colnames(merital_dummies) <- gsub("^MARITAL", "MARITAL_", colnames(merital_dummies))

# Dodanie zmiennych do zbioru danych
wesbrook2 <- cbind(wesbrook2, as.data.frame(merital_dummies))
wesbrook2$MARITAL <- NULL # usunięcie oryginalnej zmiennej MARITAL

# Usun pomocna przy wizualizacji zmienną WESBROOK_original
wesbrook2$WESBROOK_original <- NULL

cat("\nStruktura zbioru wesbrook2 po przygotowaniu danych:\n")
str(wesbrook2)

# Podzielenie danych metodą stratyfikowanego próbkowania
library(caTools)

set.seed(1234)
train_val_set <- sample.split(wesbrook2$WESBROOK, SplitRatio = 0.8)
wesbrook2_stratified_train <- subset(wesbrook2, train_val_set == TRUE)
wesbrook2_stratified_val <- subset(wesbrook2, train_val_set == FALSE) # zbiór walidacyjny

cat("\nWymiary zbioru treningowego:", dim(wesbrook2_stratified_train), "\n")
cat("Wymiary zbioru walidacyjnego:", dim(wesbrook2_stratified_val), "\n")

cat("\nProporcje zmiennej WESBROOK w zbiorze treningowym:\n")
print(table(wesbrook2_stratified_train$WESBROOK) / nrow(wesbrook2_stratified_train))
cat("Proporcje zmiennej WESBROOK w zbiorze walidacyjnym:\n")
print(table(wesbrook2_stratified_val$WESBROOK) / nrow(wesbrook2_stratified_val))

# Zapisanie wyników
write_csv(wesbrook2_stratified_train, "data/lab-1/wesbrook_train.csv")
write_csv(wesbrook2_stratified_val, "data/lab-1/wesbrook_val.csv")

cat("\nAnaliza i przygotowanie danych zakończone pomyślnie.\n")


