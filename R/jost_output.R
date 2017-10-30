#' Jost output classes
#'
#' Creates jost_output class
#' @keywords jost
#' @export
#' @examples
#' jost_output()

jost_output <- setClass("jost.output",
                        slots = c(summary = "numeric",
                                  weights = "numeric",
                                  spec.freq = "numeric",
                                  boot = "logical",
                                  boot.output = "list"))

