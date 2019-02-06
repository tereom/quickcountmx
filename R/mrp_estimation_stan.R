#' Postratified estimation for the number of votes for several candidates.
#'
#' The function fits a model using the \code{rstan} package and
#' predicts proportional of votes for all candidates candidate in unobserved polling
#' stations. Optionally the model can be fit with a stratified random sample
#' @param ... One or more unquoted expressions separated by commas, indicating 
#' the column names with the votes for each candidate.
#' @inheritParams mrp_estimation_stan
#' @param n_warmup burnin size
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @importFrom rstan sampling extract
#' @export
mrp_estimation_stan <- function(data, ..., stratum, frac = 1,  n_iter = 500,
    n_warmup = 250, n_chains = 1, seed = NA, 
    parallel = FALSE, n_cores = 1, model_string = NULL,
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
            
        } else {
            
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
                    n_chains      = n_chains
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
