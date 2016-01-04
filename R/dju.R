library(data.table)
library(lubridate)
library(zoo)

data_path = "D:/DATA/f.pouchain/Desktop/ICADE/data/weather"
dt = fread(file.path(data_path, "ForecastIO_Aubervilliers_2012-01-01_2014-12-31.csv"))

dt[, time := fast_strptime(time, "%Y-%m-%d %H:%M:%S", tz = "CET")]
dt[, year_month := format(time, "%Y-%m")]

dt = dt[year_month != "2015-01"]


temp = 0:30
above = paste0("above_",temp)
below = paste0("below_",temp)

compute_degree_days = function(air_temperature,
                               direction = "above",
                               base_temperature) {
  if (direction == "above") {
    unlist(lapply(base_temperature, function(x) {sum(air_temperature[air_temperature > x] - x, na.rm = T)}))
  } else {
    unlist(lapply(base_temperature, function(x) {sum(air_temperature[air_temperature < x] - x, na.rm = T)}))
  }
}

# cdd = dt[, compute_degree_days(temperature, "above", temp), by = "year_month" ]
# hdd = dt[, compute_degree_days(temperature, "below", temp), by = "year_month" ]
# setnames(cdd, c("year_month", above))
# setnames(hdd, c("year_month", below))



mean_temp = dt[, list(mean_temp = mean(temperature),
                      cdd = compute_degree_days(temperature, "above", 20),
                      hdd = compute_degree_days(temperature, "below", 15)),
               by = "year_month"]

setnames(mean_temp, c("year_month", "mean_temp", "cdd", "hdd"))
mean_temp[, month := format(fast_strptime(year_month, format = "%Y-%m"), "%m")]

temp_2015 = mean_temp[, list(year_month = paste0("2015-",month),
                             mean_temp = mean(mean_temp),
                             cdd = mean(cdd),
                             hdd = mean(hdd)),
                      by = "month"]
temp_2015 = temp_2015[, c("year_month", "mean_temp",  "cdd", "hdd", "month"), with = F]
mean_temp = rbind(mean_temp, temp_2015)



temp_2030 = fread("D:/DATA/f.pouchain/Desktop/ICADE/data/weather/ICADE_Parc_Porte_Paris-hour_HEURE_2030A2.txt")
temp_2050 = fread("D:/DATA/f.pouchain/Desktop/ICADE/data/weather/ICADE_Parc_Porte_Paris-hour_HEURE_2050A2.txt")

temp_2050[, T16 := (Ta - 16)/(10-16)]
plot(temp_2050$T16, type = 'l')

temp_2030[, date := seq(as.POSIXct("2030-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
                        as.POSIXct("2030-12-31 23:00:00", format = "%Y-%m-%d %H:%M:%S"), by = 3600)]

temp_2050[, date := seq(as.POSIXct("2050-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
                        as.POSIXct("2050-12-31 23:00:00", format = "%Y-%m-%d %H:%M:%S"), by = 3600)]

temp_2030[, month := format(date, "%m")]
temp_2050[, month := format(date, "%m")]

temp_2030 = temp_2030[, list(year_month = paste0("2030-",month),
                             mean_temp = mean(Ta),
                             cdd = compute_degree_days(Ta, "above", 20),
                             hdd = compute_degree_days(Ta, "below", 15)),
                      by = "month"]
temp_2030 = temp_2030[, c("year_month", "mean_temp", "cdd","hdd", "month"), with = F]

temp_2050 = temp_2050[, list(year_month = paste0("2050-",month),
                             mean_temp = mean(Ta),
                             cdd = compute_degree_days(Ta, "above", 20),
                             hdd = compute_degree_days(Ta, "below", 15)),
                      by = "month"]
temp_2050 = temp_2050[, c("year_month", "mean_temp", "cdd","hdd", "month"), with = F]


mean_temp = rbind(mean_temp, temp_2030, temp_2050)

# p = ggplot(mean_temp)
# p = p + geom_line(aes(x = year_month, y = mean_temp, group = 1))
# p


# degree_days = merge(cdd, hdd, by = "year_month")
# 
# setwd(data_path)
# write.table(degree_days, "degree_days.csv", sep = ",", row.names = F)
# 
# 
# 
# 
# C = fread("C_test.csv")
# C = as.data.table(t(C))
# setnames(C, "C")
# d.temp = cbind(mean_temp, C)


mean_temp[, year := format(fast_strptime(year_month, format = "%Y-%m"), "%Y")]
mean_temp[, month := format(fast_strptime(year_month, format = "%Y-%m"), "%m")]

mean_temp.d = dcast(mean_temp, month ~ year, value.var = "mean_temp")
mean_temp.d[, `2025` := as.numeric(NA)]
mean_temp.d[, `2035` := as.numeric(NA)]

mean_temp = melt(mean_temp.d, id.vars = "month", variable.name = "year", value.name = "mean_temp")
mean_temp[, mean_temp := na.approx(mean_temp, x = as.numeric(levels(year))[year], xout = as.numeric(levels(year))[year]), by = month]

mean_temp[, sup_13 := ifelse(mean_temp - 13 > 0, mean_temp - 13, 0)]
mean_temp[, inf_10 := ifelse(mean_temp - 10 < 0, mean_temp - 10, 0)]


mean_temp[, K_corr_froid := sup_13/100*0.84*0.75]
mean_temp[, K_corr_chaud := inf_10/100*0.75]

sum(mean_temp$K_corr_froid + mean_temp$K_corr_chaud)
plot(mean_temp$K_corr_froid + mean_temp$K_corr_chaud, type = 'l')


mean_temp[, year_month := paste0(year,"-",month)]
mean_temp[, year := NULL]
mean_temp[, month := NULL]

write.table(mean_temp, "mean_temp.csv", row.names = F)




