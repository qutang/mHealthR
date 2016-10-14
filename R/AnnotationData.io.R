#' @name AnnotationData.io.writeCsv
#' @title Write annotation data into mhealth folder structure and with mhealth filename convention.
#' @export
#' @import lubridate stringr
#' @param folder the output folder
#' @param annotationData the input dataframe that matches mhealth specification.
#' @param ontologyId the ontology ID for this set of annotations
#' @param annotatorId the annotator's ID for this set of annotations
#' @param tz the time zone string (P/MHHMM) used in filename.
#' @param gzip whether to gzip the output csv file.
#' @param flatDir whether to use mhealth folder structure or just use flat directory.
#' @param splitHour whether to split input dataframe into hourly csv files.
#' @param custom_name if provided, the file name will be custom_name and all other file name preset parameters such as sensorType will be discarded.
#' @param append whether to append to a file if the file exists
#' @param header whether to add column header or not
AnnotationData.io.writeCsv = function(folder,
                                      annotationData,
                                      ontologyId = NA,
                                      annotatorId = NA,
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
      startTime = annotationData[1, 1]
      utcTime = stringr::ymd_hms(startTime)
      localTime = startTime
      hourDiff = round(utcTime - localTime, digits = 2)
      tzStr = utils.formatTimeZone(hourDiff)
    } else{
      tzStr = tz
    }
    timeStamp = strftime(startTime, format = mhealth.FILE_TIMESTAMP_PATTERN, origin = origin)
    timeStamp = stringr::str_replace(timeStamp, pattern = "\\.", replacement = "-")
    timeStampStr = paste(timeStamp, tzStr, sep = "-")
    section1 = ontologyId
    section2 = annotatorId
    annotationFilename = paste(section1, section2, timeStampStr, "annotation", "csv", sep = ".")
  } else{
    annotationFilename = custom_name

  }
  if (!flatDir) {
    containFolder = file.path(folder,
                              year(startTime),
                              month(startTime),
                              day(startTime),
                              hour(startTime))
  } else{
    containFolder = folder
  }
  dir.create(containFolder,
             showWarnings = FALSE,
             recursive = TRUE)
  fullPath = file.path(containFolder, annotationFilename)
  options(digits.secs = 3)
  if (gzip) {
    gzFile = gzfile(
      description = paste(fullPath, "gz", sep = "."),
      open = "w",
      compression = 6,
      encoding = "UTF-8"
    )
    write.table(
      x = annotationData,
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
      x = annotationData,
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

#' @name AnnotationData.io.importCsv
#' @title Import mhealth annotation data file and load into memory as data frame in mhealth format.
#' @export
#' @param filename full file path for input annotation data file.
#' @note Time zone is from local computer for now. But should be changed to use filename in the future.
AnnotationData.io.importCsv = function(filename, violate = FALSE) {
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
    if (!grepl("annotation.csv", filename))
      stop("Please make sure the raw data file is in annotaiton.csv or annotation.csv.gz format")
  }
  # read.table supports csv.gz directly
  dat = read.table(
    filename,
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE,
    fill = TRUE,
    row.names = NULL
  )
  # TODO: use the time zone specified in the filename
  dat[, 1] = as.POSIXct(strptime(dat[, 1], format = mhealth.TIMESTAMP_PATTERN))
  dat[, mhealth.START_TIME_COLUMN] = as.POSIXct(strptime(dat[, mhealth.START_TIME_COLUMN], format = mhealth.TIMESTAMP_PATTERN))
  dat[, mhealth.STOP_TIME_COLUMN] = as.POSIXct(strptime(dat[, mhealth.STOP_TIME_COLUMN], format = mhealth.TIMESTAMP_PATTERN))
  return(dat)
}
