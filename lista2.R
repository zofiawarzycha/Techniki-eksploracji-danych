#zadanie 1 
daneZ1 <- c(-10, 9, 1, 2, 5, -2, 6, 2, 1, 0, 1, 4, 5, 6, 3, 7, 3, 2, 2, 3, 8,
             5, 3, 4, 8, 0, 8, 0, 5, 1, 6, 4, 8, 13, 2, -13, 20)
mean_Z1 <- mean(daneZ1)
median_Z1 <- median(daneZ1)

install.packages(DecsTools)
library(DescTools)
mode_Z1 <- mode(daneZ1)
range_Z1 <- range(daneZ1)
variance_Z1 <- var(daneZ1)
standarddeviation_Z1 <- sd(daneZ1)
quantile_Z2 <- quantile(daneZ1, 0.5)
quantile_Z2
median_Z1

#zadanie 2
hist(daneZ1, breaks = 10, main = "Histogram danych z zadania 1", 
     xlab = "Wartości danych", ylab = "Częstość", col = "pink", 
     border = "hotpink")
box()

#zadanie 3
boxplot(daneZ1, breaks = 1, main = "Wykres pudełko-wąsy dla danych z zadania 1", 
        xlab = "Dane", ylab = "Wartości", col = "lightblue", border = "blue")

#zadanie 4
dolna_granica <- mean_Z1 - 3*standarddeviation_Z1
gorna_granica <- mean_Z1 + 3*standarddeviation_Z1
odstajace_sigma <- daneZ1[daneZ1<dolna_granica|daneZ1>gorna_granica]

z4Q1 <- quantile(daneZ1, 0.25)
z4Q3 <- quantile(daneZ1, 0.75)
z4IQR <- z4Q3 - z4Q1
dolne_IQR <- z4Q1 - 1.5*z4IQR
gorne_IQR <- z4Q3 + 1.5*z4IQR
odstajace_IQR <- daneZ1[daneZ1<dolne_IQR|daneZ1>gorne_IQR]

odstajace_sigma
odstajace_IQR

#zadanie 5
library(ggplot2)
data("mpg")
head(mpg)
hwy_dane <- mpg$hwy
hwy_dane

mean_hwy <- mean(hwy_dane)
median_hwy <- median(hwy_dane)

install.packages(DecsTools)
library(DescTools)
mode_hwy <- mode(hwy_dane)
range_hwy <- range(hwy_dane)
variance_hwy <- var(hwy_dane)
standarddeviation_hwy <- sd(hwy_dane)
quantile_hwy <- quantile(hwy_dane)

#zadanie 6
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(bins = 10, fill = "pink", color = "hotpink") +
  ggtitle("Histogram dla hwy (10 przedziałów)") +
  theme_minimal()

ggplot(mpg, aes(x = hwy)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "blue") +
  ggtitle("Histogram dla hwy (20 przedziałów)") +
  theme_minimal()

ggplot(mpg, aes(y = hwy)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Wykres pudełkowy dla hwy") +
  theme_minimal()

#zadanie 7
hwy_dolna_granica <- mean_hwy - 3*standarddeviation_hwy
hwy_gorna_granica <- mean_hwy + 3*standarddeviation_hwy
hwy_odstajace_sigma <- hwy_dane[hwy_dane<hwy_dolna_granica|hwy_dane>hwy_gorna_granica]

hwy_Q1 <- quantile(hwy_dane, 0.25)
hwy_Q3 <- quantile(hwy_dane, 0.75)
hwy_IQR <- hwy_Q3 - hwy_Q1
hwy_dolne_IQR <- hwy_Q1 - 1.5*hwy_IQR
hwy_gorne_IQR <- hwy_Q3 + 1.5*hwy_IQR
hwy_odstajace_IQR <- hwy_dane[hwy_dane<hwy_dolne_IQR|hwy_dane>hwy_gorne_IQR]

hwy_odstajace_sigma
hwy_odstajace_IQR
indeksy_odstajace_sigma <- which(hwy_dane < hwy_dolna_granica | hwy_dane > hwy_gorna_granica)
indeksy_odstajace_sigma
indeksy_odstajace_IQR <- which(hwy_dane<hwy_dolne_IQR|hwy_dane>hwy_gorne_IQR)
indeksy_odstajace_IQR

#zadanie 8
X <- c(1, 5, 10, 8, 9, 1, 2, 4, 5, 6)
Y <- c(120, 115, 132, 123, 128, 102, 106, 109, 112, 110)

plot(X, Y,
     main = "Zależność między stażem pracy a wydajnością",
     xlab = "Staż pracy (lata)",
     ylab = "Wydajność",
     pch = 19, col = "hotpink")
abline(lm(Y ~ X), col = "blue")
cor(X,Y)

#zadanie 9
install.packages("ggpubr")
library(ggpubr)
data("mtcars")

cor_z9 <- cor(mtcars$mpg, mtcars$wt)
cor_z9

mrl_z9 <- lm(mpg ~ wt, data = mtcars)
summary(mrl_z9)

ggscatter(mtcars, x = "wt", y = "mpg", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Waga (wt)", ylab = "Miles per Gallon (mpg)")

#zadanie 10
install.packages("expsmooth")
library(expsmooth)

plot(bonds, main = "Szereg czasowy: bonds", ylab = "Wartość", xlab = "Czas")
plot(visitors, main = "Szereg czasowy: visitors", ylab = "Wartość", xlab = "Czas")

windows(width = 10, height = 7) #otwiera okno i bardziej widać
lag.plot(bonds, lags = 4, main = "Lag plot dla bonds")
lag.plot(visitors, lags = 4, main = "Lag plot dla visitors")

library(forecast)
model_bonds <- tslm(bonds ~ trend)
summary(model_bonds)

model_visitors <- tslm(visitors ~ trend)
summary(model_visitors)

forecast_bonds <- forecast(model_bonds, h = 4)
windows(width = 13, height = 9) #otwiera okno i bardziej widać
plot(forecast_bonds)

forecast_visitors <- forecast(model_visitors, h = 4)
windows(width = 13, height = 9) #otwiera okno i bardziej widać
plot(forecast_visitors)
