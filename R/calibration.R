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
                              n_rep = 5, model_string = NULL, num_missing_strata = 0){
  party_enquo <- dplyr::enquo(party)
  stratum_enquo <- dplyr::enquo(stratum)
  data_enquo <- dplyr::enquo(data)
  actual <- dplyr::pull(data, !!party_enquo) 
  # set up cluster
  clust <-  parallel::makeCluster(getOption("cl.cores", cl_cores))
  parallel::clusterSetRNGStream(clust, seed)
  parallel::clusterExport(clust, c("frac", "n_iter", "n_burnin", "n_chains", "actual",
                                   "model_string", "cl_cores",
                                   "stratum_enquo", "party_enquo", "num_missing_strata"), 
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
                model_string = model_string, num_missing_strata = num_missing_strata
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
                        cl_cores = 3, n_rep = 5, 
                        model_string = NULL, num_missing_strata = 0){
  stratum_enquo <- dplyr::enquo(stratum)
  parties <- dplyr::quos(...)
  # get names
  parties_name <- data %>% dplyr::select(!!!parties) %>% names
  # gather data for calculations
  strata <- unique(data %>% dplyr::pull(!!stratum_enquo))
  dat_gather <- data %>%
    tidyr::gather(party, n_votes, !!!parties)
  actual <-dat_gather %>% dplyr::group_by(party) %>% 
    dplyr::summarise(n_votes = sum(n_votes)) %>%
    dplyr::mutate(prop_votes = 100*n_votes/sum(n_votes))
  clust <-  parallel::makeCluster(getOption("cl.cores", cl_cores))
  parallel::clusterSetRNGStream(clust, seed)
  parallel::clusterExport(clust, c("frac", "n_iter", "n_burnin", "n_chains", "actual",
                                   "cl_cores", "model_string",
                                   "stratum_enquo", "parties", "data",
                                   "num_missing_strata","strata"), 
                          envir = environment())
  parallel::clusterEvalQ(clust, {
    library(dplyr)
    library(quickcountmx)
  })
  clb <- parallel::parLapply(clust, 1:n_rep, function(x){
     not_selected <- integer(0)
     if(num_missing_strata > 0){
        not_selected <- sample(strata, num_missing_strata)
     }
     mrp <- mrp_estimation(data, !!!parties, frac = frac, 
                stratum = !!stratum_enquo, n_iter = n_iter, n_burnin = n_burnin, 
                n_chains = n_chains, seed = NA, parallel = TRUE,
                model_string = NULL, set_strata_na = not_selected)
     df <- mrp$post_summary %>% dplyr::left_join(actual) %>% 
          dplyr::mutate(n_sim = x)
      df
  })
  parallel::stopCluster(clust)
  dplyr::bind_rows(clb)
}

#' @rdname calibration
#' @export
summary_calibration_party <- function(calib_run_party, alpha_r = 0.05) {
  cal_summary <- calib_run_party %>% group_by(n_sim) %>%
    summarise(mean_post = mean(n_votes), 
              inf = mean(n_votes) - 2*sd(n_votes), 
              sup = mean(n_votes) + 2*sd(n_votes), 
              actual_votes = actual_votes[1])
  # plot
  plot_calib <- ggplot(cal_summary, aes(x = factor(n_sim), 
                                        y = mean_post, ymin = inf, ymax = sup)) + 
    geom_hline(aes(yintercept = actual_votes[1]), colour = 'red') +
    geom_hline(aes(yintercept = mean(cal_summary$mean_post))) +
    geom_point() + geom_linerange()
  # coverage report
  coverage_report <- cal_summary %>% 
    summarise(coverage = mean(inf < actual_votes & sup > actual_votes))
  
  list(plot = plot_calib, coverage = coverage_report)
}

#' @rdname calibration
#' @export
summary_calibration <- function(calib_run, alpha_r = 0.05) {
    means_party <- calib_run %>% dplyr::group_by(party) %>% 
    dplyr::summarise(mean_party = mean(mean_post), prop_votes = mean(prop_votes))
    # plot
    plot_calib <- ggplot(calib_run, 
      aes(x = n_sim, ymin = mean_post - 2 * std_dev_post,
          ymax = mean_post + 2 * std_dev_post)) +
      geom_linerange(colour='red') + 
      facet_wrap(~party, scales = 'free_y') +
      geom_hline(data = means_party, aes(yintercept = mean_party), colour ='orange') +
      geom_hline(data = means_party, aes(yintercept = prop_votes), colour ='black')
    # coverage report
    coverage_report <- calib_run %>% 
      dplyr::mutate(covered = ((mean_post - 2 * std_dev_post) < prop_votes & 
        (mean_post + 2 * std_dev_post) > prop_votes)*1) %>% 
      dplyr::group_by(party) %>% 
      dplyr::summarise(coverage = mean(covered), 
          n_sims = length(covered),
          mean_error = mean(2*std_dev_post))
  list(plot = plot_calib, coverage = coverage_report)
}
