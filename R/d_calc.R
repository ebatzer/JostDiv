#' Jost diversity calculation
#'
#' Creates d_calc function that calculates Jost diversity for a dataframe
#' @keywords jost
#' @export
#' @examples
#' d_calc()

d_calc <- function(com.matrix, q, sample.weight){
  com.sums <- 0

  if(is.vector(com.matrix)){
    if( q == 1){
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums - sum(temp.matrix * log(temp.matrix))

      div <- exp(com.sums)

    }else{
      temp.matrix <- com.matrix
      temp.matrix <- temp.matrix[temp.matrix != 0]
      com.sums <- com.sums + sum(temp.matrix ^ q)

      div <- (com.sums / sum(sample.weight^q)) ^ (1 / (1 - q))

    }
  }else{
    if( q == 1){
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums -(sample.weight[i] * sum(temp.matrix * log(temp.matrix)))
      }

      div <- exp(com.sums)

    }else{
      for( i in 1:dim(com.matrix)[1]){
        temp.matrix <- com.matrix[i, ]
        temp.matrix <- temp.matrix[temp.matrix != 0]
        com.sums <- com.sums + (sample.weight[i] ^ q) * (sum(temp.matrix ^ q))
      }

      div <- (com.sums / sum(sample.weight^q)) ^ (1 / (1 - q))

    }
  }


  return(div)
}
