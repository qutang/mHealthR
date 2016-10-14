#' @name utils.formatTimeZone
#' @title format numerical value of hour difference into time zone string in mhealth specification
#' @return time zone string in format "[M/P]HHmm"
#' @export
utils.formatTimeZone = function(hourDiff) {
  if (hourDiff < 0) {
    prefix = "M"
  } else{
    prefix = "P"
  }
  hourStr = as.character(floor(abs(hourDiff)))
  if (hourDiff < 10) {
    hourStr = paste0("0", hourStr)
  }
  minDiff = abs(hourDiff) - floor(abs(hourDiff))
  if (minDiff == 0) {
    result = paste0(prefix, hourStr, "00")
  } else if (minDiff == 0.5) {
    result = paste0(prefix, hourStr, "30")
  } else if (minDiff == 0.25) {
    result = paste0(prefix, hourStr, "15")
  } else if (minDiff == 0.75) {
    result = paste0(prefix, hourStr, "45")
  } else{
    result = paste0(prefix, hourStr, "00")
  }
  return(result)
}

#' @name utils.parseTimeZone
#' @title convert time zone string in mhealth specification back to numerical value of hour difference
#' @return numerical value of hourly difference
#' @import stringr
#' @export
utils.parseTimeZone = function(tzStr) {
  tokens = stringr::str_match(tzStr, "([0-9]{2})(([0-9]{2}))")
  hourInt = as.numeric(tokens[1, 2])
  hourFrag = as.numeric(tokens[1, 3]) / 60
  hourDiff = hourInt + hourFrag
  if (stringr::str_detect(tzStr, "M")) {
    hourDiff = -hourDiff
  }
  return(hourDiff)
}
