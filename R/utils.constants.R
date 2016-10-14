#' @export
mhealth.TZ_PATTERN = "[MP]+[0-9]{4}"

#' @export
mhealth.FILE_TIMESTAMP_PATTERN = "%Y-%m-%d-%H-%M-%OS"

#' @export
mhealth.TIMESTAMP_PATTERN = "%Y-%m-%d %H:%M:%OS"

#' @export
mhealth.TIMESTAMP_COLUMN = "HEADER_TIME_STAMP"

#' @export
mhealth.START_TIME_COLUMN = "START_TIME"

#' @export
mhealth.STOP_TIME_COLUMN = "STOP_TIME"

#' @export
mhealth.ACCEL_COLUMNS_IN_G = c('X_IN_G', 'Y_IN_G', 'X_IN_G')

#' @export
actigraph.TIMESTAMP_PATTERN = "%m/%d/%Y %H:%M:%OS"

#' @export
actigraph.IMU_TIMESTAMP_PATTERN = "%Y-%m-%dT%H:%M:%OS"

actigraph.SR_PATTERN = "Sample Rate: ([0-9]+)"
actigraph.FIRMWARE_PATTERN = "Firmware: ([0-9]+.[0-9]+.[0-9]+)"
actigraph.SERIALNUM_PATTERN = "Serial Number: ([A-Za-z0-9]+)"
actigraph.DEVICETYPE_PATTERN = "Device Type: ([A-Za-z0-9]+)"
actigraph.STARTDATE_PATTERN = "Start Date: ([0-9]+)"
actigraph.DOWNLOADTIME_PATTERN = "Download Date: ([0-9]+)"
actigraph.GRANGE_PATTERN = "Acceleration Max: ([0-9\\.]+)"
actigraph.SOFTWARE_PATTERN = "ActiLife v([0-9]+.[0-9]+.[0-9]+)"
