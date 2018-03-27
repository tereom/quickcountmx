#' Compute all postratified estimates
#'
#' This function calls \code{\link{mrp_party_estimation}} using all the parties
#' specified in `...`, once there is an estimation of the number of votes for
#' each party/candidate it computes the proportion of votes for each one.
#' @param ... One or more unquoted expressions separated by commas, indicating
#'   the column names with the votes for each candidate.
#' @inheritParams mrp_party_estimation
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
mrp_estimation <- function(data, ..., frac_sample = 1,
    n_iter = 2000, n_burnin = 500, n_chains = 3, seed = NA){
    if(is.na(seed)){seed = sample(1:100)}
    parties <- dplyr::quos(...)
    data_long <- tidyr::gather(data, party, n_votes, !!!parties)
    parties_models <- data_long %>%
        split(.$party) %>%
        purrr::map(~mrp_party_estimation(., party = n_votes,
            stratum = distrito_loc_17, frac_sample = frac_sample,
            n_chains = n_chains, n_iter = n_iter, n_burnin = n_burnin))
    jags_fits <- purrr::map(parties_models, ~.$fit)
    votes_all <- purrr::map_df(parties_models, ~.$n_votes) %>%
        dplyr::mutate(n_sim = 1:n()) %>%
        tidyr::gather(party, votes, -n_sim) %>%
        dplyr::group_by(n_sim) %>%
        dplyr::mutate(
            total = sum(votes),
            prop = votes/total
            )
    post_summary <- votes_all %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(
            mean_post = 100 * mean(prop),
            median_post = 100 * median(prop),
            std_dev_post = 100 * sd(prop)
            ) %>%
        dplyr::arrange(desc(mean_post))
    return(list(jags_fits = jags_fits, post_summary = post_summary))
}
