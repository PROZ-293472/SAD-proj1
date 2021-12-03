library(ggplot2)

setwd("C:\\Users\\danad\\Desktop\\repos\\studies\\S2 second-cycle\\SAD\\proj1")

catastrophes <- read.csv(file = "./data/katastrofy.csv")
catastrophes$Date <- as.Date(catastrophes[["Date"]], format = "%m/%d/%Y")
catastrophes$Quarters <- lubridate::quarter(catastrophes$Date, with_year = TRUE)

ggplot(catastrophes, aes(x = Quarters)) +
  geom_histogram(binwidth = 1)

cat_subset <- catastrophes[between(catastrophes$Date, as.Date("1940-01-01"), as.Date("1956-01-01")),]

poisson <- as.data.frame(dpois(15:30, lambda = 24))
poisson$x <- 1940:1955
colnames(poisson) <- c("y", "x")

ggplot(cat_subset, aes(x = Quarters)) +
  geom_histogram(binwidth = 1) +
  geom_point(data = poisson, aes(x, y * 1000), color = "red") +
  scale_y_continuous(
    name = "Liczba wypadków",
    sec.axis = sec_axis(trans = ~. * 0.001, name = "Rozkład Poissona")
  )

flights <- read.csv(file = "./data/number-of-flights-from-2.csv", sep = ";")
colnames(flights) <- c("Category", "Flights")

ggplot(flights, aes(x = Category, y = Flights)) +
  geom_point()

years <- lubridate::year(catastrophes$Date)
year_catastrophes <- as.data.frame(table(years[years > 2003]))
year_catastrophes$Flights <- flights[1:6, 2]
colnames(year_catastrophes) <- c("Year", "Catastrophes", "Flights")

ggplot(year_catastrophes, aes(x = Year, group = 1)) +
  geom_line(aes(y = Catastrophes), size = 2, color = "blue") +
  geom_line(aes(y = Flights * 2), size = 2, color = "green") +
  scale_y_continuous(
    name = "Katastrofy",
    sec.axis = sec_axis(trans = ~. * 0.5, name = "Loty w milionach")
  )
