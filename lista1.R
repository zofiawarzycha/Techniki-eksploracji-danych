#zadanie 1 
data(package="datasets")

install.packages("forecast")
library(forecast)
data(package="forecast")
library(help="forecast")

install.packages("expsmooth")
library(forecast)
data(package="expsmooth")

#zadanie 2
data("AirPassengers")
class(AirPassengers) #time series
str(AirPassengers)
frequency(AirPassengers)
start(AirPassengers)
end(AirPassengers)
AirPassengers
deltat(AirPassengers)
time(AirPassengers)
cycle(AirPassengers)

#zadanie 3
plot(AirPassengers, main = "Liczba pasażerów liniii lotniczych (1950-1960)", 
     type = "l", col = "hotpink", lty = 1, lwd = 2, xlab = "Rok", 
     ylab = "Liczba pasażerów", axes = FALSE)
grid(col = "lightgray", lty = "dotted")
axis(side = 1, at = seq(1949, 1961, by = 1), cex.axis = 0.8, las = 1)
axis(side = 2, at = seq(100, 600, by = 50), cex.axis = 0.8, las = 2)
box()

#zadanie 4
scanz4 <- scan("z4tekst.txt", dec = ",", sep = " ")
scanz4
class(scanz4)
ts_scanz4 <- ts(scanz4, start = c(2010, 1), frequency = 4)
ts_scanz4

daneZ4 <- seq(100, 600, by = 50)
daneZ4
write.table(daneZ4, file = "daneZ4.txt", row.names = FALSE, col.names = FALSE)
wczytanie_daneZ4 <- scan("daneZ4.txt")
class(wczytanie_daneZ4)

szereg_daneZ4 <- ts(wczytanie_daneZ4, start = c(2023, 1), frequency = 4)
szereg_daneZ4

numerycznie_daneZ4 <- as.numeric(szereg_daneZ4)
numerycznie_daneZ4

tekstowo_daneZ4 <- as.character(szereg_daneZ4)
tekstowo_daneZ4

#zadanie 5
csvZ5 <- read.csv("csvZ5.csv", header = TRUE, sep = ";")
csvZ5
head(csvZ5)

csvZ5 <- read.table("csvZ5.csv", header = TRUE, sep = ";")

samelata_csvZ5 <- as.numeric(csvZ5[1, -c(1,2)])
samelata_csvZ5

ts_csvZ5 <- ts(samelata_csvZ5, start = c(2010, 1), frequency = 1)
ts_csvZ5

#zadanie 6
txtZ6 <- read.table("badanie1.txt", header = TRUE, sep = " ")
min(txtZ6$wynagro)

tsZ6 <- ts(txtZ6$wynagro, start = c(1640, 1), frequency = 1)
tsZ6
summary(tsZ6)
plot(tsZ6)

#zadanie 7
csvZ7 <- read.csv("z7.csv", header = TRUE, sep = ";")
csvZ7
samelata_csvZ7 <- as.numeric(csvZ7[1, -c(1,2)])
ts_csvZ7 <- ts(samelata_csvZ7, start = c(1999, 1), frequency = 1)
ts_csvZ7

class(ts_csvZ7)
is.ts(ts_csvZ7)
tsp(ts_csvZ7)
attributes(ts_csvZ7)

#zadanie 8
dane72 <- rnorm(72)
ts12_dane72 <- ts(dane72, start = c(2010, 1), frequency = 12)
ts4_dane72 <- ts(dane72, start = c(2010, 1), frequency = 4)
ts1_dane72 <- ts(dane72, start = c(2010, 1), frequency = 1)

#zadanie 9
ramkadanych <- data.frame(
  Imię = as.character(c("Jan", "Ignacy", "Hania")),
  Wiek = as.numeric(c(25, 27, 22)),
  Ranking = factor(c(1, 6, 4), levels = c(1,2,3,4,5,6), ordered = TRUE)
)
class(ramkadanych)
class(ramkadanych$Imię)
class(ramkadanych$Wiek)
class(ramkadanych$Ranking)

nowywiersz <- c("Zosia", 20, 4)
ramkadanych <- rbind(ramkadanych, nowywiersz)
Płeć <- as.factor(c("kobieta", "mężczyzna","niebinarny", "kobieta"))
ramkadanych <- cbind(ramkadanych, Płeć)
ramkadanych

str(ramkadanych)
class(ramkadanych$Płeć)
levels(ramkadanych$Ranking)

ncol(ramkadanych)
dim(ramkadanych)
head(ramkadanych, 2)
tail(ramkadanych, 2)

#zadanie 10
data("AirPassengers")
podzbiorAS <- window(AirPassengers, start = c(1950, 1), end = c(1957, 12))

plot(podzbiorAS, main = "Liczba pasażerów liniii lotniczych (1950-1957)", 
     type = "l", col = "hotpink", lty = 1, lwd = 2, xlab = "Rok", 
     ylab = "Liczba pasażerów", axes = FALSE)
grid(col = "lightgray", lty = "dotted")
axis(side = 1, at = seq(1950, 1958, by = 1), cex.axis = 0.8, las = 1)
axis(side = 2, at = seq(100, 600, by = 50), cex.axis = 0.8, las = 2)
box()

#zadanie 11
install.packages("forecast")
library(forecast)

install.packages("expsmooth")
library(expsmooth)
data("usgdp")

granica <- round(length(usgdp)*0.7)
granica
uczaca <- head(usgdp, granica)
testowa <- tail(usgdp, length(usgdp)-granica)

czescuczaca <- window(uczaca, start = c(1947, 1))
czesctestowa <- window(testowa, start = c(1989, 1))
czescuczaca
czesctestowa

ts.plot(czescuczaca, czesctestowa, main = "Podział danych US GDP: część ucząca i testowa",
        col = c("hotpink", "purple"), lty = c(1, 2), lwd = 2, xlab = "Rok",
        ylab = "PKB (mld USD)")
grid(col = "lightgray", lty = "dotted")
legend("topleft", legend = c("Część ucząca", "Część testowa"), 
       col = c("hotpink", "purple"), lty = c(1, 2), bty = "n")

#zadanie 12
install.packages("quantmod", dependencies = TRUE)
library(quantmod)

getSymbols("^DJI", src = "yahoo", from = "2000-01-01", to = Sys.Date())
getSymbols("EURUSD=X", src = "yahoo", from = "2000-01-01", to = Sys.Date())
getSymbols("BTC-USD", src = "yahoo", from = "2000-01-01", to = Sys.Date())
getSymbols("PKN.WA", src = "yahoo", from = "2021-01-01", to = Sys.Date())

DJIA <- na.omit(DJI)
EURUSD <- na.omit(`EURUSD=X`)
BTCUSD <- na.omit(`BTC-USD`)
ORLEN <- na.omit(`PKN.WA`)

head(DJIA, 4)
head(EURUSD, 4)
head(BTCUSD, 4)
head(ORLEN, 4)

plot(DJIA$DJI.Close, main = "Dow Jones Industrial Average (DJIA)", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "hotpink")
plot(EURUSD$`EURUSD=X.Close`, main = "EUR/USD Exchange Rate", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "purple")
plot(BTCUSD$`BTC-USD.Close`, main = "BTC/USD Exchange Rate", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "blue")
plot(ORLEN$PKN.WA.Close, main = "Orlen", xlab = "Data", 
     ylab = "Cena zamknięcia", type = "l", col = "green")

#zadanie 13
install.package
library(tseries)

DJIA <- get.hist.quote(instrument = "^DJI", quote = "Close", provider = "yahoo",
                       start = "2000-01-01", end = Sys.Date())
EURUSD <- get.hist.quote(instrument = "EURUSD=X", quote = "Close", provider = "yahoo",
                         start = "2000-01-01", end = Sys.Date())
BTCUSD <- get.hist.quote(instrument = "BTC-USD", quote = "Close", provider = "yahoo",
                         start = "2000-01-01", end = Sys.Date())

DJIA <- na.omit(DJIA)
EURUSD <- na.omit(EURUSD)
BTCUSD <- na.omit(BTCUSD)

plot(DJIA, main = "Dow Jones Industrial Average (DJIA)", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "lightpink")
plot(EURUSD, main = "EUR/USD Exchange Rate", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "lightgreen")
plot(BTCUSD, main = "BTC/USD Exchange Rate", 
     xlab = "Data", ylab = "Cena zamknięcia", type = "l", col = "lightblue")

head(DJIA, 4)
head(EURUSD, 4)
head(BTCUSD, 4)

#zadanie 14
data("AirPassengers")
print(AirPassengers)

lag.plot(AirPassengers, lags = 12, main = "Wykresy rozrzutu dla pełnych danych AirPassengers")

obcieteAP <- window(AirPassengers, start = c(1950, 1)) 
lag.plot(obcieteAP, lags = 12, main = "Wykresy rozrzutu dla obciętych danych AirPassengers")

#zadanie 15
set.seed(123)

bialyszum <- rnorm(1000, mean = 0, sd = 1)
plot(bialyszum, main = "Biały szum", xlab = "Czas", ylab = "Wartość", type = "l")

library(stats)
lag.plot(bialyszum, lags = 4, do.lines = FALSE, main = "Wykresy rozrzutu białego szumu")

#zadanie 16
data("AirPassengers")
roznica_AP1 <- diff(AirPassengers, lag = 1)
roznica_AP1
roznica_AP12 <- diff(AirPassengers, lag = 12)
roznica_AP12

library(stats)
lag.plot(roznica_AP1, lags = 12, do.lines = FALSE, main = "Wykresy rozrzutu dla różnic lag = 1")
lag.plot(roznica_AP12, lags = 12, do.lines = FALSE, main = "Wykresy rozrzutu dla różnic lag = 12")

#zadanie 17
library(lattice)
data("Nile")
Nile
str(Nile)

xyplot(Nile ~ time(Nile), type = "l", main = "Przepływ Nilu w latach 1871–1970", 
       xlab = "Rok", ylab = "Przepływ (jednostki)", col = "hotpink", lwd = 2)

xyplot(Nile ~ time(Nile), aspect = 1, type = "l", main = "Proporcja 1:1",
       xlab = "Rok", ylab = "Przepływ (jednostki)", col = "purple", lwd = 0.5)

xyplot(Nile ~ time(Nile), aspect = 0.5, type = "l", main = "Proporcja szeroka (0.5)",
       xlab = "Rok", ylab = "Przepływ (jednostki)", col = "green", lwd = 2)

xyplot(Nile ~ time(Nile), aspect = 2, type = "l", main = "Proporcja wąska (2)",
       xlab = "Rok", ylab = "Przepływ (jednostki)", col = "blue", lwd = 0.5)

#zadanie 18
library(lattice)
library(datasets)

data("Nile")
dane_nile <- Nile
dane_nile

ramka_nile <- data.frame(
  Year = time(Nile),
  Flow = as.vector(Nile)
)
ramka_nile

ile_lat <- length(ramka_nile$Year)
ile_lat

overlap <- 0.5
dlugosc_czesci <- round(ile_lat * (1-overlap))

czesc1 <- 1:dlugosc_czesci
czesc2 <- (dlugosc_czesci + 1 - round(overlap * dlugosc_czesci)):(dlugosc_czesci * 2)
czesc3 <- (dlugosc_czesci * 2 + 1 - round(overlap * dlugosc_czesci)):ile_lat

ramka_nile$Part <- NA
ramka_nile$Part[czesc1] <- "Część 1"
ramka_nile$Part[czesc2] <- "Część 2"
ramka_nile$Part[czesc3] <- "Część 3"
ramka_nile

xyplot(Flow ~ Year|Part, data = ramka_nile, type = "l",
       layout = c(1,3), xlab = "Year", ylab = "Flow", 
       main = "Przepływ Nilu (Podzielony na 3 Części)", auto.key = TRUE)
