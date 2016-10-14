library(curl)
h <- new_handle(copypostfields = "moo=moomooo")
handle_setheaders(h,
                  "Content-Type" = "text/csv",
                  "Cache-Control" = "no-cache",
                  "User-Agent" = "R"
)
tmp <- tempfile()
curl_download("https://timezonedb.com/files/timezonedb.csv.zip", tmp, handle = h)
timezone = data.frame(readr::read_csv(unz(tmp, filename = "timezone.csv", open = "rb"), col_names = FALSE, , na = character()))
colnames(timezone) = c("zone_id","abbreviation","time_start","gmt_offset","dst")
zone = data.frame(readr::read_csv(unz(tmp, filename = "zone.csv", open = "rb"), col_names = FALSE, na = character()))
colnames(zone) = c("zone_id","country_code","zone_name")
country = data.frame(readr::read_csv(unz(tmp, filename = "country.csv", open = "rb"), col_names = FALSE, , na = character()))
colnames(country) = c("country_code","country_name")
timezone = timezone[-c(3, 5)]

# map zone id to zone name
for(r in 1:nrow(zone)){
  timezone[timezone$zone_id == r, "zone_name"] = zone[r, 'zone_name']
  timezone[timezone$zone_id == r, "country_code"] = zone[r, 'country_code']
}

# map country code to country name
for(r in 1:nrow(country)){
  timezone[timezone$country_code == country$country_code[r], "country_name"] = country[r, 'country_name']
}

devtools::use_data(timezone, overwrite = TRUE, internal = TRUE)
