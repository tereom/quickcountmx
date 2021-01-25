#' Postratified estimation for the number of votes for several candidates.
#'
#' This function calls \code{\link{mrp_party_estimation_stan}} using all the parties
#' specified in `...`, once there is an estimation of the number of votes for
#' each party/candidate it computes the proportion of votes for each one.
#' @param ... One or more unquoted expressions separated by commas, indicating
#'   the column names with the votes for each candidate.
#' @inheritParams mrp_party_estimation_stan
#' @param parallel Logical value indicating whether to parallelize the models,
#'   if TRUE package parallel must be installed (uses mclapply and can not be
#'   used in Windows).
#' @param set_strata_na Option to exclude strata when fitting the model, used
#'   for model evaluation and calibration.
#' @param n_cores If parallelizing, the number of cores to use, parameter is
#'   used in \code{\link[parallel]{mclapply}}, \code{\link[parallel]{mclapply}}
#' @return A \code{list} with the object fitted using rstan::sampling and a
#'   data.frame with the estimation summary (posterior means, medians, standard
#'   deviations and probability intervals per party).
#' @seealso  \code{\link{mrp_party_estimation_stan}}
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @importFrom rstan sampling extract
#' @export
mrp_estimation_stan <- function(data, ..., stratum, frac = 1,  n_iter = 500,
    n_warmup = 250, n_chains = 1, seed = NA, 
    parallel = FALSE, n_cores = 1, stan_cores = 1, model_string = NULL,
    set_strata_na = integer(0)
    ){
    if (is.na(seed)) seed <- sample(1:1000, 1)
    parties <- dplyr::quos(...)
    stratum_enquo <- dplyr::enquo(stratum)
    data_long <- tidyr::gather(data, "party", "n_votes", !!!parties)
    parties_split <- data_long %>%
        split(.$party)
    if(parallel){
        if(.Platform$OS.type == "unix"){
            parties_models <- parallel::mclapply(parties_split, function(x){
                quickcountmx::mrp_party_estimation_stan(
                    x,
                    party         = n_votes,
                    stratum       = !!stratum_enquo, 
                    frac          = frac, 
                    n_iter        = n_iter,
                    warmup        = n_warmup,
                    seed          = seed,
                    model_string  = model_string,
                    set_strata_na = set_strata_na,
                    n_chains      = n_chains,
                    stan_cores    = stan_cores
                )
            },
            mc.cores = n_cores)
        } else {
            clust <-  parallel::makeCluster(getOption("cl.cores", n_cores))
            parties_split_vars <- purrr::map(
                parties_split,
                ~list(
                    data          = .,
                    party         = .$party[1],
                    stratum       = rlang::quo_text(stratum_enquo),
                    frac          = frac, 
                    n_iter        = n_iter,
                    warmup        = n_warmup,
                    seed          = seed,
                    model_string  = model_string,
                    set_strata_na = set_strata_na,
                    n_chains      = n_chains,
                    stan_cores    = stan_cores
                )
            )
            parties_models <- parallel::parLapply(
                cl = clust, 
                X = parties_split_vars, fun = function(x){		
                    quickcountmx::mrp_party_estimation_stan(
                        x$data,
                        party         = n_votes,		
                        stratum       = !!rlang::sym(x$stratum),
                        frac          = x$frac, 
                        n_iter        = x$n_iter,
                        warmup        = x$n_warmup,
                        seed          = x$seed,
                        model_string  = x$model_string,
                        set_strata_na = x$set_strata_na,
                        n_chains      = x$n_chains,
                        stan_cores    = x$stan_cores
                    )
                }
            )
            parallel::stopCluster(clust)
        }
    } else { 
        parties_models <- parties_split %>% 
            purrr::map(
                ~mrp_party_estimation_stan( 
                    .,
                    party         = n_votes,
                    stratum       = !!stratum_enquo, 
                    frac          = frac, 
                    n_iter        = n_iter,
                    warmup        = n_warmup,
                    seed          = seed,
                    model_string  = model_string,
                    set_strata_na = set_strata_na,
                    n_chains      = n_chains,
                    stan_cores    = stan_cores
                )                
            )
    }
    
    stan_fits <- purrr::map(parties_models, ~.$fit)
    votes_all <- purrr::map_df(parties_models, ~.$y) %>% 
        dplyr::mutate(n_sim = 1:(dplyr::n())) %>% 
        tidyr::gather(party, votes, -n_sim) %>% 
        dplyr::group_by(n_sim) %>%
        dplyr::mutate(
            total = sum(votes),
            prop = votes / total
        )
    participation <- dplyr::data_frame(
        party = "participacion",
        total = votes_all %>% dplyr::ungroup() %>% dplyr::pull(total), 
        prop = total / sum(data$ln)
        )
    post_summary <- votes_all %>%
        dplyr::bind_rows(participation) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(
            mean_post = 100 * mean(prop),
            median_post = 100 * median(prop),
            std_dev_post = 100 * sd(prop),
            int_l = max(0, mean_post - 1.96 * std_dev_post),
            int_r = min(100, mean_post + 1.96 * std_dev_post)
        ) %>%
        dplyr::arrange(desc(mean_post))
    
    
    return(list(stan_fits = stan_fits, post_summary = post_summary))
}
