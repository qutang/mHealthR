#' @export
mhealth = list(
  column = list(
    TIMESTAMP = "HEADER_TIMESTAMP",
    START_TIME = "START_TIME",
    STOP_TIME = "STOP_TIME",
    ANNOTATION_NAME = "LABEL_NAME",
    PARTICIPANT_ID = "PARTICIPANT_ID"
  ),
  filetype = list(
    sensor = "sensor",
    annotation = "annotation",
    event = "event",
    feature = "feature",
    log = "log",
    note = "note",
    class = "class",
    stat = "stat"
  ),
  format = list(
    filename = list(
      TIMESTAMP = "%Y-%m-%d-%H-%M-%OS"
    )
  ),
  pattern = list(
    filename = list(
      TIMESTAMP = "[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{3}",
      TIMEZONE = "[MP]+[0-9]{4}",
      FILENAME = "[A-Za-z0-9\\-]+\\.[A-Z0-9\\-]+\\.[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{3}-[MP]+[0-9]{4}\\.(sensor|annotation|event|feature|log|note|class|stat)\\.csv(\\.gz)*",
      FILETYPE = "sensor|annotation|event|feature|log|note|class|stat"
    )
  ),
  example = list(
    filename = list(
      sensor = "SensorType-DataType.SENSORID-DATATYPE-VERSIONCODE.YYYY-MM-DD-HH-mm-ss-SSS-[M/P]HHmm.sensor.csv(.gz)",
      feature = "FeatureType.FEATUREID-VERSIONCODE.YYYY-MM-DD-HH-mm-ss-SSS-[M/P]HHmm.feature.csv[.gz]",
      event = "SensorType-EventType.SENSORID-EVENTTYPE-VERSIONCODE.YYYY-MM-DD-HH-mm-ss-SSS-[M/P]HHmm.event.csv",
      annotation = "AnnotationSet.ANNOTATIONSETID-ANNOTATORID-VERSIONCODE.YYYY-MM-DD-HH-mm-ss-SSS-[M/P]HHmm.annotation.csv"
    )
  )
)
