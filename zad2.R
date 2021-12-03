library(ggplot2)

setwd("C:\\Users\\danad\\Desktop\\repos\\studies\\S2 second-cycle\\SAD\\proj1")

catastrophes <- read.csv(file = "./data/katastrofy.csv")
catastrophes$Date <- as.Date(catastrophes[["Date"]], format = "%m/%d/%Y")
catastrophes$Quarters <- lubridate::quarter(catastrophes$Date, with_year = TRUE)

ggplot(catastrophes, aes(x = Quarters)) + 
  geom_histogram(binwidth = 1, color="black", fill="white") + 
  labs(title="Katastrofy lotnicze w kolejnych kwartałach", x="Data", y = "Liczba katastrof") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

cat_subset <- catastrophes[between(catastrophes$Date, as.Date("1940-01-01"), as.Date("1956-01-01")),]

poisson <- as.data.frame(dpois(15:30, lambda = 24))
poisson$x <- 1940:1955
colnames(poisson) <- c("y", "x")

ggplot(cat_subset, aes(x = Quarters)) +
  geom_histogram(binwidth = 1, color="black", fill="white") +
  geom_point(data = poisson, aes(x, y * 1000), color = "firebrick") +
  scale_y_continuous(
    name = "Liczba wypadków",
    sec.axis = sec_axis(trans = ~. * 0.001, name = "Rozkład Poissona")
  ) +
  labs(title="Modelowanie z wykorzystaniem rozkładu Poissona", x="Kwartały") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

flights <- read.csv(file = "./data/number-of-flights-from-2.csv", sep = ";")
colnames(flights) <- c("Category", "Flights")

ggplot(flights, aes(x = Category, y = Flights)) + 
  geom_point() +
  labs(title="Liczba odbywających się lotów", x="Lata", y="Loty") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

years <- lubridate::year(catastrophes$Date)
year_catastrophes <- as.data.frame(table(years[years > 2003]))
year_catastrophes$Flights <- flights[1:6, 2]
colnames(year_catastrophes) <- c("Year", "Catastrophes", "Flights")

ggplot(year_catastrophes, aes(group = 1)) +
  geom_line(aes(x = Year, y = Catastrophes, colour="Katastrofy"), size = 1) +
  geom_line(aes(x = Year, y = Flights * 2, colour="Loty"), size = 1) +
  scale_y_continuous(
    name = "Katastrofy lotnicze",
    sec.axis = sec_axis(trans = ~. * 0.5, name = "Loty (w milionach)")
  ) +
  labs(title="Loty i katastrofy lotnicze w kolejnych latach", x="Lata") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Legenda", values = c("Katastrofy" = "firebrick", "Loty" = "dodgerblue"))
