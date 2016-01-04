
get_forecastIO = function(lon, lat, from, to, api_key, variables = c("time", "pressure", "temperature", "humidity", "windSpeed", "windBearing")) {

  # Time frame definition
  date_list = seq(as.Date(from), as.Date(to), by = "day")
  date_list = as.character(date_list)

  # Loop the API requests
  for (i in 1:length(date_list)) {

    # Select the date and formulate API request
    time = date_list[[i]]
    message(paste("Getting data for", time))
    url = paste("https://api.forecast.io/forecast/",api_key,"/",lat,",",lon,",",time,"T00:00:00", "?units=si", sep = "")
    message(paste("API call:",url))

    # Expand iteratively the results with each api response
    json = jsonlite::fromJSON(url)
    if (i == 1) {data = json$hourly$data}
    else {data = merge(data, json$hourly$data, all.x = T, all.y = T)}

    # Let the API breathe betwen each request
    Sys.sleep(0.5)

  }

  # Format data
  data$time = as.POSIXlt(data$time, origin = "1970-01-01")

  # Filter dimensions (for available dimensions, see : https://developer.forecast.io/docs/v2)
  data = data[, variables]

  closeAllConnections()

  return(data)

}
