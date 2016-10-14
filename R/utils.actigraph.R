#' @name utils.parseActigraphHeader
#' @title parse actigraph header string as list
#' @import stringr R.utils
utils.parseActigraphHeader = function(filename, column_names = TRUE) {
  headlines = readLines(filename, n = 10, encoding = "UTF-8")

  # Sampling rate
  sr_pattern = actigraph.SR_PATTERN
  sr = headlines[[1]]
  sr = str_match(sr, sr_pattern)
  sr = as.numeric(sr[2])

  # Firmware code
  fw_pattern = actigraph.FIRMWARE_PATTERN
  fw = headlines[[1]]
  fw = str_match(fw, fw_pattern)
  fw = fw[2]

  # Software code
  sw_pattern = actigraph.SOFTWARE_PATTERN
  sw = headlines[[1]]
  sw = str_match(sw, sw_pattern)
  sw = sw[2]

  # Serial number
  sn_pattern = actigraph.SERIALNUM_PATTERN
  sn = headlines[[2]]
  sn = str_match(sn, sn_pattern)
  sn = sn[2]

  # actigraph type
  at = substr(sn, 1, 3)

  # g range
  gr = switch(
    at,
    MAT = "3",
    CLE = "6",
    MOS = "8",
    TAS = "8"
  )

  # IMU or not
  if (str_detect(headlines[[1]], "IMU")) {
    imu = TRUE
  } else{
    imu = FALSE
  }

  if (imu) {
    gr = "16"
  }

  # Session start time
  st = headlines[[3]]
  sd = headlines[[4]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+"

  dateReg = "[0-9]+/[0-9]+/[0-9]{4}"

  st = regmatches(st, regexpr(timeReg, st, perl = TRUE))
  sd = regmatches(sd, regexpr(dateReg, sd, perl = TRUE))
  st = paste(sd, st, sep = ' ')
  timeFormat = actigraph.TIMESTAMP_PATTERN
  st = strptime(st, timeFormat) + 0.0005
  options(digits.secs = 3)


  # Session download time
  dt = headlines[[6]]
  dd = headlines[[7]]
  timeReg = "[0-9]{2}(:[0-9]{2}){1,2}+"

  dateReg = "[0-9]+/[0-9]+/[0-9]{4}"

  dt = regmatches(dt, regexpr(timeReg, dt, perl = TRUE))
  dd = regmatches(dd, regexpr(dateReg, dd, perl = TRUE))
  dt = paste(dd, dt, sep = ' ')
  timeFormat = actigraph.TIMESTAMP_PATTERN
  dt = strptime(dt, timeFormat) + 0.0005
  options(digits.secs = 3)


  if (is.na(sr)) {
    # determine sr by start and download time
    # options(digits = 13)
    duration = as.numeric(dt - st, units = "secs")
    if (column_names) {
      nlines = countLines(filename) - 11
    } else{
      nlines = countLines(filename) - 10
    }
    sr = as.numeric(ceiling(nlines / duration))
  }

  # input voltage
  vs = headlines[[9]]
  vsReg = ": ([0-9](\\.[0-9]+)*)"
  vs = as.numeric(str_match(vs, vsReg)[2])

  # input resolution
  resolution = headlines[[9]]
  resReg = "= ([0-9]+)"
  resolution = as.numeric(str_match(resolution, resReg)[2])

  # header object as output
  header = {

  }
  header$sr = sr
  header$fw = fw
  header$sw = sw
  header$sn = sn
  header$st = st
  header$dt = dt
  header$at = at
  header$imu = imu
  header$gr = gr
  header$vs = vs
  header$res = resolution

  return(header)
}

#' @name SensorData.formatActigraphHeader
#' @title create a character vector representing each line of the actigraph csv header
#' @import stringr
utils.formatActigraphHeader = function(startTime,
                                               downloadTime,
                                               samplingRate,
                                               sensorId,
                                               firmVersion,
                                               softVersion) {
  line = c(
    "------------ Data File Created By ActiGraph GT3X+ ActiLife vSOFT_VERSION Firmware vFIRM_VERSION date format M/d/yyyy at SAMPLING_RATE Hz  Filter Normal -----------"
  )
  line = stringr::str_replace(line, "SOFT_VERSION", softVersion)
  line = stringr::str_replace(line, "FIRM_VERSION", firmVersion)
  line = stringr::str_replace(line, "SAMPLING_RATE", samplingRate)

  line = c(line, stringr::str_replace("Serial Number: ID", "ID", sensorId))
  line = c(line,
           stringr::str_replace(
             "Start Time START_TIME",
             "START_TIME",
             format(startTime, "%H:%M:%S")
           ))
  line = c(line,
           stringr::str_replace(
             "Start Date START_DATE",
             "START_DATE",
             format(startTime, "%m/%d/%Y")
           ))
  line = c(line, "Epoch Period (hh:mm:ss) 00:00:00")
  line = c(line,
           stringr::str_replace(
             "Download Time DOWNLOAD_TIME",
             "DOWNLOAD_TIME",
             format(downloadTime, "%H:%M:%S")
           ))
  line = c(line,
           stringr::str_replace(
             "Download Date DOWNLOAD_DATE",
             "DOWNLOAD_DATE",
             format(downloadTime, "%m/%d/%Y")
           ))
  line = c(line, "Current Memory Address: 0")
  line = c(line, "Current Battery Voltage: 4.19     Mode = 12")
  line = c(line,
           "--------------------------------------------------")
  line = c(line, ACTIGRAPH_HEADER_COLUMNS)
  return(line)
}
