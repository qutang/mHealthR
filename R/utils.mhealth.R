#' @name mhealth.parseAnnotatorId
#' @title Get annotator Id from annotation file name
#' @import stringr
#' @export
mhealth.parseAnnotatorId = function(filename){
  tokens = stringr::str_split(filename, "\\.")
  tokens = stringr::str_split(tokens[[1]][2], "-")
  return(tokens[[1]][1])
}

#' @name mhealth.parseOntologyId
#' @title Get ontology Id from annotation file name
#' @import stringr
#' @export
mhealth.parseOntologyId = function(filename){
  tokens = stringr::str_split(filename, "\\.")
  return(tokens[[1]][1])
}
