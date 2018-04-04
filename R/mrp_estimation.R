#' Compute all postratified estimates
#'
#' This function calls \code{\link{mrp_party_estimation}} using all the parties
#' specified in `...`, once there is an estimation of the number of votes for
#' each party/candidate it computes the proportion of votes for each one.
#' @param ... One or more unquoted expressions separated by commas, indicating
#'   the column names with the votes for each candidate.
#' @inheritParams mrp_party_estimation
#' @parallel Logical value indicating whether to parallelize the models, if TRUE
#'   package parallel must be installed.
#' @clust If parallelizing an object of class \code{c("SOCKcluster", "cluster")}
#' as returned by \code{\link[parallel]{makeCluster}}.
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
mrp_estimation <- function(data, ..., stratum, frac = 1,
    n_iter = 2000, n_burnin = 500, n_chains = 3, seed = NA, parallel = FALSE,
    clust){
    if (is.na(seed)) seed <- sample(1:1000, 1)
    parties <- dplyr::quos(...)
    stratum_enquo <- dplyr::enquo(stratum)
    data_long <- tidyr::gather(data, "party", "n_votes", !!!parties)
    parties_split <- data_long %>%
        split(.$party)
    if (parallel){
        parties_models <- parallel::parLapply(clust, parties_split, function(x)
            quickcount::mrp_party_estimation(data = x, party = n_votes,
            stratum = rlang::`!!`(stratum_enquo), frac = frac,
                n_chains = n_chains, n_iter = n_iter, n_burnin = n_burnin,
                seed = seed)
            )
    } else {
        parties_models <- parties_split %>%
            purrr::map(~mrp_party_estimation(., party = n_votes,
                stratum = !!stratum_enquo, frac = frac,
                n_chains = n_chains, n_iter = n_iter, n_burnin = n_burnin,
                seed = seed, parallel = parallel))
    }

    jags_fits <- purrr::map(parties_models, ~.$fit)
    votes_all <- purrr::map_df(parties_models, ~.$n_votes) %>%
        dplyr::mutate(n_sim = 1:n()) %>%
        tidyr::gather(party, votes, -n_sim) %>%
        dplyr::group_by(n_sim) %>%
        dplyr::mutate(
            total = sum(votes),
            prop = votes / total
            )
    post_summary <- votes_all %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(
            mean_post = 100 * mean(prop),
            median_post = 100 * median(prop),
            std_dev_post = 100 * sd(prop),
            int_l = mean_post - 1.96 * std_dev_post,
            int_r = mean_post + 1.96 * std_dev_post,
            ) %>%
        dplyr::arrange(desc(mean_post))
    return(list(jags_fits = jags_fits, post_summary = post_summary))
}
