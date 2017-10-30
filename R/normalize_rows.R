#' Normalize rows function
#'
#' Normalizes rows for diversity calculations
#' @keywords jost
#' @export
#' @examples
#' normalize_rows()

normalize_rows <- function(com.matrix){

  row.totals <- rowSums(com.matrix)

  com.matrix <- as.matrix(com.matrix)

  for(i in 1:nrow(com.matrix)){
    com.matrix[i, ] <- com.matrix[i, ] / row.totals[i]
  }

  return(com.matrix)
}
