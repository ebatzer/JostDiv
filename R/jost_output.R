#' Jost output classes
#'
#' Creates jost_output class
#' @keywords jost
#' @export
#' @examples
#' jost_output()

jost_output <- setClass("jost.output",
                        slots = c(summary = "list",
                                  weights = "numeric",
                                  spec.freq = "data.frame",
                                  boot = "logical",
                                  boot.output = "list"))

