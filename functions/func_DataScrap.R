# functions for DataScrap.R

scrapZipCsvData <- function(url,...){
  # create temporary file to store zip file
  tmp_file <- tempfile()
  # download file
  download.file(url,tmp_file)
  # unzip file
  raw_data <- unzip(tmp_file)
  # read the contents
  data <- read.csv(raw_data, ...)
  # delete downloaded file
  file.remove(raw_data)
  
  return(data)
}

fredr::fredr_set_key(suppressWarnings(read.delim(file="fredrKey.txt",header=FALSE)$V1))
fredr <- function(
  series,
  observation_start = NULL,
  observation_end = NULL,
  frequency = NULL,
  aggregation_method = NULL,
  limit = NULL,
  offset = NULL,
  sort_order = NULL,
  units = NULL,
  realtime_start = NULL,
  realtime_end = NULL,
  vintage_dates = NULL,
  output_type = NULL,
  ...
){
  purrr::map_dfr(
    series, function(series_id) fredr::fredr(series_id = series_id,
                                             observation_start = observation_start,
                                             observation_end = observation_end,
                                             frequency = frequency,
                                             aggregation_method = aggregation_method,
                                             limit = limit,
                                             offset = offset,
                                             sort_order = sort_order,
                                             units = units,
                                             realtime_start = realtime_start,
                                             realtime_end = realtime_end,
                                             vintage_dates = vintage_dates,
                                             output_type = output_type,
                                             ...)
  )
}

tq_get <- function(x, get = "stock.prices", complete_cases = TRUE, ...){
  purrr::map_dfr(
    x, function(x_i){
      data <- tryCatch(
        tidyquant::tq_get(x_i, get = get, complete_cases = complete_cases, ...) %>% 
          group_by(date) %>% slice(1) %>% ungroup(),
        error   = function(e) cat("Error while downloading ",x_i,"\n", sep=""),
        warning = function(e) cat("Warning while downloading ",x_i,"\n", sep="")
      )
      return(data)
    }
  )
}
