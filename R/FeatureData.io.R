#' @name FeatureData.io.writeCsv
#' @title Write sensor data into mhealth folder structure and with mhealth filename convention.
#' @import lubridate stringr
#' @param folder the output folder
#' @param featureData the input dataframe that matches mhealth specification.
#' @param featureType the feature type string used in filename.
#' @param sensorId the sensor ID string used in filename.
#' @param featureId the feature ID string used in filename.
#' @param versionCode the version code string used in filename; default is "NA".
#' @param tz the time zone string (P/MHHMM) used in filename.
#' @param gzip whether to gzip the output csv file.
#' @param flatDir whether to use mhealth folder structure or just use flat directory.
#' @param splitHour whether to split input dataframe into hourly csv files.
#' @param custom_name if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
#' @param append whether to append to a file if the file exists
#' @param header whether to add column header or not
FeatureData.io.writeCsv = function(folder,
                                   featureData,
                                   featureType = NA,
                                   sensorId = NA,
                                   featureId = NA,
                                   versionCode = "NA",
                                   tz,
                                   gzip = TRUE,
                                   flatDir = TRUE,
                                   splitHour = FALSE,
                                   custom_name,
                                   append = FALSE,
                                   header = TRUE) {
  if (missing(custom_name)) {
    # TODO: support split hour
    if (missing(tz)) {
      warning("Use local time zone in the file name")
      startTime = featureData[1, 1]
      utcTime = lubridate::ymd_hms(startTime)
      localTime = startTime
      hourDiff = round(utcTime - localTime, digits = 2)
      tzStr = utils.formatTimeZone(hourDiff)
    } else{
      tzStr = tz
    }
    timeStamp = strftime(startTime, format = mhealth.FILE_TIMESTAMP_PATTERN, origin = "1970-01-01")
    timeStamp = stringr::str_replace(timeStamp, pattern = "\\.", replacement = "-")
    timeStampStr = paste(timeStamp, tzStr, sep = "-")
    section1 = featureType
    section2 = paste(sensorId, featureId, versionCode, sep = "-")
    featureFilename = paste(section1, section2, timeStampStr, "feature", "csv", sep = ".")
  } else{
    featureFilename = custom_name
  }

  # get numeric columns
  numeric_cols = sapply(featureData[1,], is.numeric)

  featureData[, numeric_cols] = round(featureData[, numeric_cols], digits = 8)
  if (!flatDir) {
    containFolder = file.path(
      folder,
      lubridate::year(startTime),
      lubridate::month(startTime),
      lubridate::day(startTime),
      lubridate::hour(startTime)
    )
  } else{
    containFolder = folder
  }
  dir.create(containFolder,
             showWarnings = FALSE,
             recursive = TRUE)
  fullPath = file.path(containFolder, featureFilename)
  options(digits.secs = 3)
  if (gzip) {
    gzFile = gzfile(
      description = paste(fullPath, "gz", sep = "."),
      open = "w",
      compression = 6,
      encoding = "UTF-8"
    )
    write.table(
      x = featureData,
      file = gzFile,
      append = append,
      quote = FALSE,
      sep = ",",
      na = "",
      row.names = FALSE,
      col.names = header
    )
    close(gzFile)
  } else{
    write.table(
      x = featureData,
      file = fullPath,
      append = append,
      quote = FALSE,
      sep = ",",
      na = "",
      row.names = FALSE,
      col.names = header
    )
  }
}

#' @name FeatureData.io.importCsv
#' @title Import mhealth feature data file and load into memory as data frame in mhealth format.
#' @note input file must match mhealth specification. Note that the time zone of timestamps will be based on local computer instead of the filename, this needs to be changed.
#' @param filename full file path of input sensor data file.
#' @param violate violate file name convention, ignore time zones and other information in file name
#' @import readr
FeatureData.io.importCsv = function(filename, violate = FALSE) {
  options(digits.secs = 3)
  # get the time zone from filename
  if (!violate) {
    tz = gregexpr(pattern = mhealth.TZ_PATTERN,
                  text = filename,
                  perl = TRUE)
    tz = regmatches(filename, tz)[[1]]
    tz = gsub(pattern = "M",
              replacement = "-",
              x = tz)
    tz = gsub(pattern = "P",
              replacement = "+",
              x = tz)
    if (!grepl("csv", filename))
      stop("Please make sure the raw data file is
           in csv or csv.gz format")
  }
  # read.table supports csv.gz directly
  ncols = readr::count_fields(filename, tokenizer_csv(), n_max = 1)

  # set all columns as characters at first
  colTypes = paste(c("c", rep("c", ncols - 1)), collapse = "")
  dat = readr::read_csv(
    filename,
    col_names = TRUE,
    trim_ws = TRUE,
    col_types = colTypes
  )
  # TODO: use the time zone specified in the filename
  dat[,1:3] = as.POSIXct(strptime(dat[,1:3], format = mhealth.TIMESTAMP_PATTERN))
  names(dat)[1:3] = c(mhealth.TIMESTAMP_COLUMN,
                      mhealth.START_TIME_COLUMN,
                      mhealth.STOP_TIME_COLUMN)

  # infer numeric columns
  for (i in 4:ncol(dat)) {
    if (is.na(as.numeric(dat[1, i]))) next;
    dat[i] = as.numeric(dat[, i])
  }
  dat = data.frame(dat)
  return(dat)
}

#' @name FeatureData.io.importActigraphCountCsv
#' @title Import and convert Actigraph count csv files and load into data frame as in mhealth format.
#' @import readr
#' @param filename full file path of input Actigraph count csv file.
FeatureData.io.importActigraphCountCsv = function(filename, axes = c(2, 3, 4), count_col_name = "ACTIGRAPH_COUNT") {
  dat = readr::read_csv(
    filename,
    col_names = TRUE,
    col_types = cols(
      timestamp = col_character(),
      axis1 = col_double(),
      axis2 = col_double(),
      axis3 = col_double()
    )
  )
  dat = data.frame(dat)
  dat[, 1] = as.POSIXct(dat[, 1], format = mhealth.TIMESTAMP_PATTERN, tz = Sys.timezone())
  result = mhealth.transform(df = dat, columns = axes, drop_old = TRUE, transform_func = transform.magnitude)
  colnames(result) = c(mhealth.TIMESTAMP_COLUMN, count_col_name)
  result[, mhealth.START_TIME_COLUMN] = result[, 1]
  result[, mhealth.STOP_TIME_COLUMN] = result[, mhealth.START_TIME_COLUMN] + 5
  result = result[c(mhealth.TIMESTAMP_COLUMN, mhealth.START_TIME_COLUMN, mhealth.STOP_TIME_COLUMN, count_col_name)]
  return(result)
}
