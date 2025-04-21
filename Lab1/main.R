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