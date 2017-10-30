#' Jost diversity summary
#'
#' Uses Jost diversity calculations to get diversity values for alpha, beta, and gamma diversity of a dataset
#' @keywords jost
#' @export
#' @examples
#' d_summarize()

d_summarize <- function(com.matrix, q, sample.weight){

  # Calculating alpha diversity
  alpha.div <- d_calc(com.matrix, q = q, sample.weight)

  # Calculating gamma diversity
  gamma.matrix <- com.matrix * sample.weight
  gamma.matrix <- colSums(com.matrix) / sum(colSums(com.matrix))
  gamma.div <- d_calc(gamma.matrix, q = q, sample.weight = 1)

  # Calculating additive beta diversity
  beta.div <- gamma.div / alpha.div

  div.summary <- list(c(mean(alpha.div), beta.div, gamma.div),
                      gamma.matrix)

  names(div.summary[[1]]) = c("alpha", "beta", "gamma")
  names(div.summary[[2]]) = colnames(com.matrix)

  return(list(div.summary))

}
