# custom timestamp formatter
format_timestamp <- function(indf, index = 1) {

  if (class(indf[[index]])[1] == "POSIXlt") return(indf)

  if (stri_detect_regex(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%Y-%m-%d %H:%M", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%m/%d/%y", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%m/%d/%Y", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{4}\\d{2}\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%Y%m%d", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format = "%Y/%m/%d/%H", tz = "UTC")
  } else if (stri_detect_regex(indf[[index]][1], "^\\d{10}$")) {
    indf[[index]] <- as.POSIXlt(indf[[index]], origin = "1970-01-01", tz = "UTC") # Handle Unix seconds in milliseconds
  }

  return(indf)

}

# determine the granularity of the time series
get_gran <- function(tsdf, index=1) {

  n <- length(tsdf[[index]])

  # We calculate the granularity from the time difference between the last 2 entries (sorted)
  gran <- round(difftime(max(tsdf[[index]]), sort(tsdf[[index]], partial = n - 1)[n - 1],
    units = "secs"
  ))

  if (gran >= 86400) return("day")
  if (gran >= 3600) return("hr")
  if (gran >= 60) return("min")
  if (gran >= 1) return("sec")

  return("ms")

}
