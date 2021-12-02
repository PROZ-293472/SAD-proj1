rm(list = ls())

library(plyr)

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

dfs_list <- list()
for (i in c(1:12)){
  f_dir <- sprintf("./data/2020_%02d_k/k_d_%02d_2020.csv", i, i)
  dfs_list[[i]] <- read.csv(file = f_dir, header = FALSE)
}


df <- ldply(dfs_list, data.frame)
colnames(df) <- schema


df_tatry <- df[df$station_name == "DOLINA PIĘCIU STAWÓW", ]

mean_snow_cover <- aggregate(df_tatry$snow_cover_height, list(df_tatry$month), mean)

boxplot(snow_cover_height ~ month, data = df_tatry, 
        col='lightgoldenrod1', ylab='Wysokość pokrywy śnieżnej [cm]', xlab='Miesiąc',
        main="Wykres wysokości pokrywy śnieżnej w Tatrach w 2020")
lines(mean_snow_cover, type = 'b', col='brown1', lwd=3, pch=19, lty=2)

tatry_june <- df_tatry[df_tatry$month == "6", ]
tatry_dec <- df_tatry[df_tatry$month == "12", ]

plot(tatry_june$day, tatry_june$snow_cover_height, type = 'b', 
     col='red', lwd=3, pch=15, lty=2, 
     ylab="Wysokość pokrywy śnieżnej [cm]", xlab="Dzień", main= "Wysokość pokrywy śnieżnej w czerwcu 2020")

plot(tatry_dec$day, tatry_dec$snow_cover_height, type = 'b', 
     col='red', lwd=3, pch=15, lty=2, 
     ylab="Wysokość pokrywy śnieżnej [cm]", xlab="Dzień", main= "Wysokość pokrywy śnieżnej w grudniu 2020")
