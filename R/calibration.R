#' Title
#'
#' @param data 
#' @param party 
#' @param stratum 
#' @param frac 
#' @param n_iter 
#' @param n_burnin 
#' @param n_chains 
#' @param seed 
#' @param cl_cores 
#' @param n_rep 
#'
#' @return
#'
#' @examples
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @name calibration
NULL
#> NULL
#' @rdname calibration
#' @export
calibration_party <- function(data, party, stratum, frac = 1,
                              n_iter = 2000, n_burnin = 500, n_chains = 3, seed = NA, cl_cores = 14,
                              n_rep = 5, model_string = NULL){
  party_enquo <- dplyr::enquo(party)
  stratum_enquo <- dplyr::enquo(stratum)
  data_enquo <- dplyr::enquo(data)
  actual <- dplyr::pull(data, !!party_enquo) 
  # set up cluster
  clust <-  parallel::makeCluster(getOption("cl.cores", cl_cores))
  parallel::clusterSetRNGStream(clust, seed)
  parallel::clusterExport(clust, c("frac", "n_iter", "n_burnin", "n_chains", "actual",
                                   "model_string", "cl_cores",
                                   "stratum_enquo", "party_enquo"), 
                          envir = environment())
  parallel::clusterExport(clust, dplyr::quo_name(data_enquo))
  parallel::clusterEvalQ(clust, {
    library(tidyverse)
    library(quickcountmx)
  })
  # run replicates
  clb_party <- parallel::parLapply(clust, 1:n_rep, function(x){
    counts <- mrp_party_estimation(data, 
                party = !!party_enquo, frac = frac, stratum = !!stratum_enquo, 
                n_iter = n_iter, n_burnin = n_burnin, 
                n_chains = n_chains, seed = NA, 
                model_string = model_string
                )
    df <- dplyr::data_frame(n_votes = counts$n_votes, n_sim = x)
    df
  })
  parallel::stopCluster(clust)
  dplyr::bind_rows(clb_party) %>% dplyr::mutate(actual_votes = sum(actual))
}

#' @rdname calibration
#' @export
calibration_prop <- function(data, ..., stratum, frac = 1, n_iter = 2000, 
                        n_burnin = 500, n_chains = 3, seed = NA, 
                        cl_cores = 3, n_rep = 5, model_string = NULL){
  stratum_enquo <- dplyr::enquo(stratum)
  parties <- dplyr::quos(...)
  gto_gather <- gto_2012 %>% dplyr::select(casilla_id, !!!parties) %>%
    tidyr::gather(party, votes, !!!parties)
  actual <- gto_gather %>% group_by(party) %>% summarise(n_votes = sum(votes)) %>%
    mutate(prop_votes = 100*n_votes/sum(n_votes))
  clust <-  parallel::makeCluster(getOption("cl.cores", cl_cores))
  parallel::clusterSetRNGStream(clust, seed)
  parallel::clusterExport(clust, c("frac", "n_iter", "n_burnin", "n_chains", "actual",
                                   "cl_cores", "model_string",
                                   "stratum_enquo", "parties", "data"), 
                          envir = environment())
  parallel::clusterEvalQ(clust, {
    library(dplyr)
    library(quickcountmx)
  })
  clb <- parallel::parLapply(clust, 1:n_rep, function(x){
     mrp <- mrp_estimation(data, !!!parties, frac = frac, 
                stratum = !!stratum_enquo, n_iter = n_iter, n_burnin = n_burnin, 
                n_chains = n_chains, seed = NA, parallel = TRUE,
                model_string = NULL)
     df <- mrp$post_summary %>% dplyr::left_join(actual) %>% 
          dplyr::mutate(n_sim = x)
      df
  })
  parallel::stopCluster(clust)
  dplyr::bind_rows(clb)
}