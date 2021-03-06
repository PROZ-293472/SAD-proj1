library(dplyr)

dir <- 'C:/Users/mszpu/Studia/SAD/projekt'
setwd(dir)

schema <- c(
  'station_id',
  'station_name',
  'year',
  'month',
  'day',
  'max_daily_temp',
  'meas_status_TMAX',
  'min_daily_temp',
  'meas_status_TMIN',
  'mean_daily_temp',
  'meas_status_STD',
  'min_temp_near_ground',
  'meas_status_TMNG',
  'daily_rainfall',
  'meas_status_SMDB',
  'rainfall_type',
  'snow_cover_height',
  'meas_status_PKSN'
  
)

df1 <- read.csv('./data/2021_07_k/k_d_07_2021.csv', header=FALSE)
#df2 <- read.csv('./data/2021_08_k/k_d_08_2021.csv', header=FALSE)

#df <- rbind(df1, df2)
df <- df1
colnames(df) <- schema

# creating date column (just in case)
to_date <- function(x) {
  ISOdate(x[1], x[2], x[3])
}
df$date <- apply(df[,c('year', 'month', 'day')], 1, to_date)


# calculating differences between max and mean temps of consecutive days 
df <- df %>% group_by(station_name) %>% mutate(max_temp_diff = max_daily_temp - lag(max_daily_temp))
df <- df %>% group_by(station_name) %>% mutate(mean_temp_diff = mean_daily_temp - lag(mean_daily_temp))

# removing nans
df$max_temp_diff[is.na(df$max_temp_diff)] <- 0
df$mean_temp_diff[is.na(df$mean_temp_diff)] <- 0

print(unique(df$station_name))

loc1 <- df[df$station_name == 'PSZCZYNA', ]
loc2 <- df[df$station_name == 'GDAŃSK-RĘBIECHOWO', ]
loc3 <- df[df$station_name == 'WARSZAWA-FILTRY', ]


# calculating metrics
mean_temps <- c()
mean_stabilities <- c()
mean_diffs <- c()

for (l in list(loc1, loc2, loc3)){
  mean_temps <- c(mean_temps, mean(l$max_daily_temp))
  mean_stabilities <- c(
    mean_stabilities,
    mean(l$max_daily_temp - l$min_daily_temp)
  )
  mean_diffs <- c(mean_diffs, mean(l$max_temp_diff))
}


plot(loc1$day, loc1$max_daily_temp, type = 'b', col='green', lwd=3, pch=15, lty=2)
lines(loc2$day, loc2$max_daily_temp, type = 'b', col='orange', lwd=3, pch=19, lty=2)
lines(loc3$day, loc3$max_daily_temp, type = 'b', col='red', lwd=2, pch=12, lty=2)

all_locs <- rbind(loc1, loc2, loc3)
boxplot(max_daily_temp ~ station_name, data = all_locs)
boxplot(max_daily_temp - min_daily_temp ~ station_name, data = all_locs)
boxplot(max_temp_diff ~ station_name, data = all_locs)


# -------------- #

x <- loc3$mean_temp_diff

h <- hist(x, breaks = 10, density = 10,
          col = "black", xlab = "Temp difference [*C]", main = 'Temp difference distribution') 

linespace <- seq(min(x), max(x), length = 100) 
yfit <- dnorm(linespace, mean = mean(x), sd = sd(x)) 
# converting probability density into frequency function
yfit <- yfit * diff(h$mids[1:2]) * length(x) 

lines(linespace, yfit, col = "red", lwd = 3)
