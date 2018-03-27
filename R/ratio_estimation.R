#' Ratio estimator to compute proportion of votes allocated to each party
#'
#' Compute ratio estimator for each candidate, standard errors are computed
#' with bootstrap resampling within each stratum and computing the standard
#' error of the samples (no corrections).
#' @details The bootstrap approach we use is not suitable
#' when the number of polling stations within a strata is small. Coverage might
#' improve if confidence intervals are constructed with ABCs or t-tables.
#' @param data \code{data.frame}
#' @param ... unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param n_stratum unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param std_errors binary value indicating whether to compute standard errors
#'  (bootstrap).
#' @param B number of bootstrap replicates used to compute standard errors.
#' @return A \code{list} with the object fitted using R2jags::jags and the vector
#'   of simulated counts per candidate.
#' @examples
#' # count number of polling stations per stratum
#' gto_stratum_sizes <- gto_2012 %>%
#'     group_by(distrito_loc_17) %>%
#'     mutate(n_stratum = n())
#' # stratified random sample (size 6%), sample size proportional to strata sizes
#' gto_sample <- select_sample_prop(gto_stratum_sizes, stratum = distrito_loc_17, 0.06)
#' gto_sample %>%
#'     ratio_estimator(stratum = distrito_loc_17, n_stratum = n_stratum, ... = pri_pvem:otros)
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
ratio_estimator <- function(data, stratum, n_stratum, std_errors = TRUE, B = 50,
    ...){
    stratum <- dplyr::enquo(stratum)
    n_stratum <- dplyr::enquo(n_stratum)
    parties <- dplyr::quos(...)
    ratios <- data %>%
        dplyr::group_by(!!stratum) %>%
        dplyr::mutate(n_h = n()) %>%
        dplyr::ungroup() %>%
        tidyr::gather(party, n_votes, !!!parties) %>%
        dplyr::mutate(
            n_aux = !!n_stratum / n_h * n_votes
        ) %>%
        dplyr::group_by(!!stratum, party) %>%
        dplyr::summarise(
            n_votes = sum(n_aux)
        ) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(y = sum(n_votes)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(r = 100 * y / sum(y)) %>%
        dplyr::select(-y)
    if(std_errors == TRUE){
        ratios_sd <- sd_ratio_estimator(data, B = B, stratum = !!stratum,
            n_stratum = !!n_stratum, ... = !!!parties)
        ratios <- left_join(ratios, ratios_sd, by = "party")
    }
    return(ratios)
}

sd_ratio_estimator <- function(data, B, stratum, n_stratum, ...){
    # B bootstrap replicates
    ratio_reps <- rerun(B, sd_ratio_estimator_aux(data, stratum = !!enquo(stratum),
        n_stratum = !!enquo(n_stratum), ... = !!!quos(...)))
    std_errors <- bind_rows(!!!ratio_reps) %>%
        group_by(party) %>%
        summarise(std_error = sd(r))
    return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_ratio_estimator_aux <- function(data, stratum, n_stratum, ...){
    sample_boot <- select_sample_prop(data, stratum = !!enquo(stratum), frac = 1,
        replace = TRUE)
    ratio_estimator(sample_boot, stratum = !!enquo(stratum),
        n_stratum = !!enquo(n_stratum), std_errors = FALSE, ... = !!!quos(...))
}
