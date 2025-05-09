options(repr.plot.width = 20, repr.plot.height = 10, repr.plot.res = 200)

library(tidyverse)
library(factoextra)
library(stats)
library(dplyr)

mallcustomers <- read_csv("http://jolej.linuxpl.info/mallcustomers.csv")
summary(mallcustomers)
head(mallcustomers)

mallcustomers$Gender <- as.factor(mallcustomers$Gender)
mallcustomers$Income <- as.numeric(gsub("[, USD]", "", mallcustomers$Income))
mallcustomers <- subset(mallcustomers, select = -CustomerID)

summary(mallcustomers)
head(mallcustomers)

# Roznica w liczbie przedstawicieli plci
pie(table(mallcustomers$Gender))

# Wyrownujemy dysproporcje -> dodajemy 24 losowe obserwacje z mężczyznami
male_customers <- subset(mallcustomers, Gender == "Male")
random_male_customers <- male_customers[sample(nrow(male_customers), 24),]
mallcustomers <- rbind(mallcustomers, random_male_customers)
summary(mallcustomers)

pie(table(mallcustomers$Gender))

# Analiza wizualna danych -> histogramy na zmiennych numerycznych
hist(mallcustomers$Age, main = "Histogram wieku klientów", xlab = "Wiek", col = "lightblue")
hist(mallcustomers$Income, main = "Histogram dochodów klientów", xlab = "Dochód", col = "lightgreen")
hist(mallcustomers$SpendingScore, main = "Histogram punktów wydatków klientów", xlab = "Punkty wydatków", col = "lightpink")

boxplot(mallcustomers$Age, main = "Wiek klientów", col = "lightblue")
boxplot(mallcustomers$Income, main = "Dochody klientów", col = "lightgreen")
boxplot(mallcustomers$SpendingScore, main = "Punkty wydatków klientów", col = "lightpink")

# Wykres korelacji
pairs(mallcustomers, main = "Macierze korelacji - Klienci Mall")
# Brak widocznych korelacji

# Klasteryzacja
# Przed klasteryzacja powinnismy dokonac normalizacji danych z racji ze income i spending score sa w roznej skali wielkosci
mallcustomers_scaled <- mallcustomers %>%
  select(SpendingScore, Income) %>%
  scale()
mallcustomers_scaled <- subset(mallcustomers_scaled,
                               select = c(Income, SpendingScore))

summary(mallcustomers_scaled)

# Klasteryzacja k = 2, 3, 4, 5
set.seed(1234)
k_means <- kmeans(mallcustomers_scaled, centers = 2, nstart = 25)
fviz_cluster(
  k_means,
  data = mallcustomers_scaled,
  repel = TRUE,
  ggtheme = theme_minimal()) + theme(text = element_text(size = 14)
)

set.seed(1234)
k_means <- kmeans(mallcustomers_scaled, center = 3, nstart = 25)
fviz_cluster(
  k_means,
  data = mallcustomers_scaled,
  repel = TRUE,
  ggtheme = theme_minimal()) + theme(text = element_text(size = 14)
)

set.seed(1234)
k_means <- kmeans(mallcustomers_scaled, center = 4, nstart = 25)
fviz_cluster(
  k_means,
  data = mallcustomers_scaled,
  repel = TRUE,
  ggtheme = theme_minimal()) + theme(text = element_text(size = 14)
)

set.seed(1234)
k_means <- kmeans(mallcustomers_scaled, center = 5, nstart = 25)
fviz_cluster(
  k_means,
  data = mallcustomers_scaled,
  repel = TRUE,
  ggtheme = theme_minimal()) + theme(text = element_text(size = 14)
)

k_means

fviz_nbclust(mallcustomers_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette Method
fviz_nbclust(mallcustomers_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap Statistic Method
# Note: This might take a bit longer to compute
set.seed(123)
gap_stat <- clusGap(mallcustomers_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(subtitle = "Gap statistic method")

# Wybór liczby klastrów
mallcustomers$cluster <- k_means$cluster

# Proporcja women - men per klaster
summary <- mallcustomers %>%
  group_by(cluster, Gender) %>%
  summarise(count = n())

summary <- summary %>%
  group_by(cluster) %>%
  # Mutacja dodaje kolumny z proporcją kobiet i mężczyzn w każdym klastrze
  mutate(
    female_ratio = count[Gender == "Female"] / sum(count),
    male_ratio = count[Gender == "Male"] / sum(count)
         )
summary <- summary %>% filter(Gender == "Female")
# summary <- subset(summary, select=-c(Gender, count))

print(summary)

# Srednia wieku w klastrze
summary <- mallcustomers %>%
  group_by(cluster) %>%
  summarise(mean_age = mean(Age), mean_income = mean(Income), mean_spending_score = mean(SpendingScore))

print(summary)




