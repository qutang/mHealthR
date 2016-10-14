#' @name mhealth.transform
#' @title transform function for mhealth type of data
#' @description use dplyr's select_helpers functions to select columns
#' @param df input data frame, supported types in mhealth specification
#' @param columns vector of selected columns that can be resolved to integer indices
#' @param new_column new column's name
#' @param drop_old return data frame only with the transformed new column
#' @param fun function that applies transformation for a row of selected columns.
#' @import dplyr
mhealth.transform = function(df, columns, new_column = NULL, drop_old = TRUE, transform_func){
  if(is.null(new_column)) new_column = deparse(substitute(transform_func))
  values = df %>% dplyr::select(columns)
  if(!is.function(transform_func)) stop("fun must be a function")
  result = values %>%
    dplyr::rowwise() %>%
    dplyr::do(name = transform_func(.)) %>% unlist() %>%
    as.data.frame()
  names(result) = c(new_column)
  result = cbind(df[1], result)
  rownames(result) = 1:nrow(result)
  return(result)
}

#' @name transform.magnitude
#' @title compute magnitude (norm 2) of a list or a vector
#' @export
transform.magnitude = function(input){
  if(!all(sapply(input, is.numeric))) stop("The input list has to be all numeric values")
  vec = as.numeric(unlist(input))
  return(sqrt(sum(vec ** 2)))
}
