#' @name SensorData.io.writeCsv
#' @title Write sensor data into mhealth folder structure and with mhealth filename convention.
#' @import lubridate stringr
#' @param folder the output folder
#' @param sensorData the input dataframe that matches mhealth specification.
#' @param sensorType the sensor type string used in filename.
#' @param dataType the data type string used in filename.
#' @param sensorId the sensor ID string used in filename.
#' @param versionCode the version code string used in filename; default is "NA".
#' @param tz the time zone string (P/MHHMM) used in filename.
#' @param gzip whether to gzip the output csv file.
#' @param flatDir whether to use mhealth folder structure or just use flat directory.
#' @param splitHour whether to split input dataframe into hourly csv files.
#' @param custom_name if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
#' @param append whether to append to a file if the file exists
#' @param header whether to add column header or not
SensorData.io.writeCsv = function(folder,
                                  sensorData,
                                  sensorType = NA,
                                  dataType = NA,
                                  sensorId = NA,
                                  versionCode = "NA",
                                  tz,
                                  gzip = TRUE,
                                  flatDir = FALSE,
                                  splitHour = TRUE,
                                  custom_name,
                                  append = FALSE,
                                  header = TRUE) {
  if (missing(custom_name)) {
    # TODO: support split hour
    if (missing(tz)) {
      warning("Use local time zone in the file name")
      startTime = sensorData[1, 1]
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
    section1 = paste(sensorType, dataType, versionCode, sep = "-")
    section2 = paste(sensorId, dataType, sep = "-")
    sensorFilename = paste(section1, section2, timeStampStr, "sensor", "csv", sep = ".")
  } else{
    sensorFilename = custom_name

  }

  sensorData[, -1] = round(sensorData[, -1], digits = 4)
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
  fullPath = file.path(containFolder, sensorFilename)
  options(digits.secs = 3)
  if (gzip) {
    gzFile = gzfile(
      description = paste(fullPath, "gz", sep = "."),
      open = "w",
      compression = 6,
      encoding = "UTF-8"
    )
    write.table(
      x = sensorData,
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
      x = sensorData,
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

#' @name SensorData.io.writeActigraphCsv
#' @title write sensor data frame into actigraph raw
SensorData.io.writeActigraphCsv = function(folder,
                                           sensorData,
                                           headerStr,
                                           custom_name) {
  filepath = file.path(folder, custom_name)
  file.remove(filepath)
  writeLines(
    text = headerStr,
    con = filepath,
    sep = "\n",
    useBytes = FALSE
  )
  sensorData[, -1] = round(sensorData[, -1], digits = 4)
  sensorData[, 1] = format(sensorData[, 1], actigraph.TIMESTAMP_PATTERN)
  write.table(
    x = sensorData,
    file = filepath,
    append = TRUE,
    quote = FALSE,
    sep = ",",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )
}

#' @name SensorData.io.importCsv
#' @title Import mhealth sensor data file and load into memory as data frame in mhealth format.
#' @note input file must match mhealth specification. Note that the time zone of timestamps will be based on local computer instead of the filename, this needs to be changed.
#' @param filename full file path of input sensor data file.
#' @param violate violate file name convention, ignore time zones and other information in file name
#' @import readr
SensorData.io.importCsv = function(filename, violate = FALSE) {
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
  colTypes = paste(c("c", rep("d", ncols - 1)), collapse = "")
  dat = readr::read_csv(
    filename,
    col_names = TRUE,
    trim_ws = TRUE,
    col_types = colTypes
  )
  # TODO: use the time zone specified in the filename
  dat = data.frame(dat)
  dat[,1] = as.POSIXct(strptime(dat[,1], format = mhealth.TIMESTAMP_PATTERN))

  return(dat)
  }

#' @name SensorData.importActivPal
#' @title Import ActivPal Raw data files and load into dataframe as mhealth format.
#' @import lubridate readr
#' @param filename full file path of input gt3x binary data file, should have extension "gt3x".
SensorData.importActivPal = function(filename, header_provided = FALSE) {
  ncols = readr::count_fields(filename, tokenizer_csv(), n_max = 1)
  colTypes = paste(rep("d", ncols), collapse = "")
  if (header_provided) {
    col_names = TRUE
  } else{
    col_names = FALSE
  }
  dat = readr::read_csv(
    filename,
    col_names = col_names,
    trim_ws = TRUE,
    col_types = colTypes
  )
  dat = dat[1:4]
  colnames(dat) = c(
    mhealth.TIMESTAMP_COLUMN,
    mhealth.ACCEL_COLUMNS_IN_G[1],
    mhealth.ACCEL_COLUMNS_IN_G[2],
    mhealth.ACCEL_COLUMNS_IN_G[3]
  )
  dat[,1] = as.POSIXct(dat[,1] * 60 * 60 * 24, origin =
                          "1899-12-30", tz = "GMT")
  dat[,1] = force_tz(dat[,1], tzone = Sys.timezone())

  # convert 12 bits values to g
  dat[2:4] = (dat[2:4] - 127) / 2 ^ 8 * 4
  options(digits.secs = 3)
  dat = data.frame(dat)
  return(dat)
}

#' @name SensorData.io.importActigraphCsv
#' @title Import and convert Actigraph raw csv files and load into data frame as in mhealth format.
#' @import readr
#' @note Please make sure the Actigraph raw csv file has timestamp included. The Actigraph raw csv file is not IMU csv file supported by GT9X.
#' @param filename full file path of input Actigraph raw csv file.
#' @param ad_convert set as TRUE only when the input Actigraph csv file is in analog quantized format and need to be converted into g value
#' @param ts_provided set as TRUE only when timestamp is provided as the first column
#' @param header_provided set as TRUE only when column header is provided
SensorData.importActigraphCsv = function(filename,
                                         ad_convert = FALSE,
                                         ts_provided = TRUE,
                                         header_provided = TRUE) {
  actigraphHeader = utils.parseActigraphHeader(filename, header_provided)
  ncols = readr::count_fields(filename,
                              tokenizer_csv(),
                              n_max = 1,
                              skip = 11)
  if (ts_provided) {
    colTypes = paste(c("c", rep("d", ncols - 1)), collapse = "")
  } else{
    colTypes = paste(rep("d", ncols), collapse = "")
  }
  if (header_provided) {
    dat = read_csv(
      filename,
      col_names = FALSE,
      skip = 11,
      trim_ws = TRUE,
      col_types = colTypes
    )

  } else{
    dat = read_csv(
      filename,
      col_names = FALSE,
      skip = 10,
      trim_ws = TRUE,
      col_types = colTypes
    )

  }

  if (!ts_provided) {
    ts_col = seq(from = actigraphHeader$st,
                 to = actigraphHeader$dt,
                 length.out = nrow(dat))
    dat = cbind(ts_col, dat)
  }

  dat = dat[, 1:4]

  names(dat) = c(
    mhealth.TIMESTAMP_COLUMN,
    mhealth.ACCEL_COLUMNS_IN_G[1],
    mhealth.ACCEL_COLUMNS_IN_G[2],
    mhealth.ACCEL_COLUMNS_IN_G[3]
  )

  if (ts_provided) {
    timeFormat = ifelse(test = actigraphHeader$imu,
                        yes = actigraph.IMU_TIMESTAMP_PATTERN,
                        no = actigraph.TIMESTAMP_PATTERN)
    dat[,1] = strptime(x = dat[,1],
                        format = timeFormat) + 0.0005
  }

  options(digits.secs = 3)

  dat = data.frame(dat)
  return(dat)
}
