degree_days = function(air_temperature,
                       direction = "above",
                       base_temperature) {
  if (direction == "above") {
    unlist(lapply(base_temperature, function(x) {sum(air_temperature[air_temperature > x] - x, na.rm = T)}))
  } else {
    unlist(lapply(base_temperature, function(x) {sum(air_temperature[air_temperature < x] - x, na.rm = T)}))
  }
}
