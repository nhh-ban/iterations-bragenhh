library(lubridate)
library(anytime)
library(jsonlite)

### 2 transforming to a tibble and converting to UTC format
transform_metadata_to_df <- function(data){
  df <- data[[1]] %>% 
  map(as_tibble) %>% 
  list_rbind() %>% 
  mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
  mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
  unnest_wider(location) %>% 
  unnest_wider(latLon)
}

### 4A
# Adding function 
to_iso8601 <- function(time, offset) { # argument date time and offset in days
  adjust_date <- iso8601(time + days(offset))
  adjust_date <- paste0(adjust_date, "Z")
  return(adjust_date) # returns date variable in iso8601 format with offset 
}                     # with Z and UTC time zone

### 5
# transforms json return from the API to the data frame
transform_volumes <- function(api_response, name = NULL) {
  edges <- api_response$trafficData$volume$byHour$edges
  #Create empty data frame 
  df <- map_dfr(edges, function(edge) {
    tibble(
      from = as_datetime(edge$node$from),
      to = as_datetime(edge$node$to),
      volume = edge$node$total$volumeNumbers$volume,
      station = name
    )
  })
  return(df)
}

