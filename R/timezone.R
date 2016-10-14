#' @name timezone.to_location
#' @title convert utc offset/utc offset in hours/utc offset in seconds to location name
#' @import stringr
#' @export
timezone.to_location = function(input, country_or_city = NULL){
  zone_id = .timezone.parse(input, country_or_city)
  candidates = unique(timezone$zone_name[zone_id])
  return(candidates)
}

#' @name timezone.to_abbr
#' @title convert utc offset/utc offset in hours/utc offset in seconds to location name
#' @import stringr
#' @export
timezone.to_abbr = function(input, country_or_city = NULL){
  zone_id = .timezone.parse(input, country_or_city)
  return(unique(timezone$abbreviation[zone_id]))
}

.timezone.parse = function(input, country_or_city = NULL){
  if(class(input) == "numeric"){
    if(abs(input) < 24){
      # in hours
      zone_id = timezone$gmt_offset == input * 3600
    }else{
      # in seconds
      zone_id = timezone$gmt_offset == input
    }
  }else if(class(input) == "character" && stringr::str_detect(input, "^(\\+|\\-)[0-1]{1}[0-9]{1}[0-5]{1}[0-9]{1}$")){
    hour = as.numeric(substr(input, 2, 3)) * 3600
    min = as.numeric(substr(input, 4,5)) * 60
    seconds = hour + min
    if(substr(input, 1, 1) == "-") seconds = -seconds
    zone_id = timezone$gmt_offset == seconds
  }else{
    stop(sprintf(
      "\n
      Input is not a valid utc offset string: %s",
      input
    ))
  }

  # filter by country or city
  if(!is.null(country_or_city)) zone_id = zone_id & (timezone$country_code == country_or_city |
                                                       stringr::str_detect(timezone$country_name, country_or_city) |
                                                       stringr::str_detect(timezone$zone_name, country_or_city))

  return(which(zone_id))
}
