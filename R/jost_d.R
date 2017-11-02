#' Jost diversity calculations
#'
#' Jost function to be called
#' @keywords jost
#' @export
#' @examples
#' jost_d()

jost_d <- function(q = 1, # Q-value to evaluate diversity with
                   com.matrix, # Community matrix
                   weighted = FALSE, # Whether samples should be weighted by the total number of individuals in each sampling unit.
                   boot = FALSE, # Whether to bootstrap
                   n.sim = NULL){ # Arguments passed to boostrap

  if(!as.numeric(q) | q <= 0 | q >= 2){
    stop("Error: q must be a numeric value between 0 and 2")
  }

  if(weighted == FALSE){
    sample.inds <- rowSums(com.matrix)
    sample.weight <- rep(1/nrow(com.matrix), nrow(com.matrix))
  }else{
    sample.inds <- rowSums(com.matrix)
    sample.weight <- sample.inds / sum(sample.inds)
  }

  # Normalizing rows
  com.matrix <- normalize_rows(com.matrix)

  div.summary <- d_summarize(com.matrix, q, sample.weight)

  if(boot == TRUE){

    boot.store <- list()
    spec.freq <- div.summary[[1]][[2]]

    for(simno in 1:n.sim){

      sim.community <- list()
      counter <- 1

      for(sample.size in sample.inds){

        sim.community[[counter]] <- t(rmultinom(n = 1,
                                                size = sample.size,
                                                prob = spec.freq))
        counter <- counter + 1

      }

      sim.community <- matrix(unlist(sim.community),
                              ncol = length(spec.freq),
                              byrow = T)

      sim.community <- normalize_rows(sim.community)


      boot.store[simno] <- c(d_summarize(sim.community, q, sample.weight)[[1]][1])
    }

    sim.alpha <- unlist(lapply(boot.store, "[[", 1))
    sim.beta <- unlist(lapply(boot.store, "[[", 2))
    sim.gamma <- unlist(lapply(boot.store, "[[", 3))

    sim.output <- data.frame(level = c("alpha", "beta", "gamma"),
                             sample.mean = c(mean(sim.alpha),
                                      mean(sim.beta),
                                      mean(sim.gamma)),
                             sample.stderror = c(sd(sim.alpha),
                                    sd(sim.beta),
                                    sd(sim.gamma)))

  }else{
    sim.output = list(NULL)
  }

  jost.out <- jost_output(summary = div.summary[[1]][1],
                        weights = sample.weight,
                        spec.freq = data.frame(spec = as.character(colnames(com.matrix)),
                                               freq = div.summary[[1]][[2]]),
                        boot = boot,
                        boot.output = sim.output)

  return(jost.out)

}
