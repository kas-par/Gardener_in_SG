



get_weather_data <- function() {
  
  weather_Website = "https://www.srf.ch/meteo/wetter/Cham/47.1822,8.4566?geolocationNameId=24582f48c13d1284979d3c1f048cbcdc"
  regenmenge = weather_Website %>%
    read_html() %>%
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "weather-hours__amount", " " ))]') %>% 
    html_text()
  regenmenge = gsub("Regenmenge","",regenmenge)
  regenmenge = as_tibble(regenmenge[1:6])
  regenmenge$value = as.numeric(regenmenge$value)
  return(regenmenge$value)
}