#' Ratio estimator to compute proportion of votes allocated to each party
#'
#' Compute ratio estimator for each candidate, standard errors are computed
#' with bootstrap resampling within each stratum and computing the standard
#' error of the samples (no corrections).
#' @details The bootstrap approach we use is not suitable
#' when the number of sampled polling stations within a strata is small.
#' Coverage might improve if confidence intervals are constructed with BCas or
#' t-tables.
#' @param data \code{data.frame}
#' @param stratum Unquoted variable indicating the stratum for each polling
#'   station.
#' @param data_stratum Data frame with stratum variable (named exactly as in 
#'   \code{data}) and number of polling stations per strata.
#' @param n_stratum Unquoted variable indicating the number of polling stations
#'   in each stratum.
#' @param ... Unquoted variables indicating the number of votes in each polling
#'   station for each candidate.
#' @param std_errors Logical value indicating whether to compute standard errors
#'  (using bootstrap), defaults to TRUE.
#' @param B Number of bootstrap replicates used to compute standard errors, 
#'  defaults to 50.
#' @param seed integer value used to set the state of the random number
#' generator (optional). It will only be used when computing standard errors.
#' @return A \code{data.frame} including the ratio estimation for each party 
#'   and standard errors (if requested). 
#' @examples
#' # count number of polling stations per stratum
#' library(dplyr)
#' gto_stratum_sizes <- gto_2012 %>%
#'     dplyr::group_by(distrito_loc_17) %>%
#'     dplyr::summarise(n_stratum = n())
#' # stratified random sample (size 6%), sample size proportional to strata size
#' gto_sample <- select_sample_prop(gto_2012, stratum = distrito_loc_17, 0.06)
#' ratio_estimation(gto_sample, stratum = distrito_loc_17, 
#'   data_stratum = gto_stratum_sizes, n_stratum = n_stratum, pri_pvem:otros)
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
ratio_estimation <- function(data, stratum, data_stratum, n_stratum, ..., 
    std_errors = TRUE, B = 50, seed = NA){
    stratum_enquo <- dplyr::enquo(stratum)
    n_stratum_enquo <- dplyr::enquo(n_stratum)
    parties_quo <- dplyr::quos(...)
    
    data_stratum <- data_stratum %>% 
        dplyr::rename(strata = !!stratum_enquo, n_strata = !!n_stratum_enquo)
    
    # calculate estimates
    data <- data %>%
        dplyr::ungroup() %>% 
        dplyr::rename(strata = !!stratum_enquo) %>% 
        dplyr::left_join(data_stratum, by = "strata")
    ratios <- data %>% 
        dplyr::group_by(strata) %>%
        dplyr::mutate(n_h = n()) %>%
        dplyr::ungroup() %>%
        tidyr::gather(party, n_votes, !!!parties_quo) %>%
        dplyr::mutate(
            n_aux = n_strata / n_h * n_votes
        ) %>%
        dplyr::group_by(strata, party) %>%
        dplyr::summarise(
            n_votes = sum(n_aux)
        ) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(y = sum(n_votes)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(r = 100 * y / sum(y)) %>%
        dplyr::select(-y)
    
    if (std_errors == TRUE) {
        ratios_sd <- sd_ratio_estimation(data, data_stratum = data_stratum, 
            B = B, !!!parties_quo)
        ratios <- dplyr::left_join(ratios, ratios_sd, by = "party") %>%
            dplyr::arrange(desc(r))
    }
    return(ratios)
}
sd_ratio_estimation <- function(data, data_stratum, ..., B){
    # B bootstrap replicates
    ratio_reps <- purrr::rerun(B, sd_ratio_estimation_aux(data = data,
        data_stratum = data_stratum, !!!dplyr::quos(...)))
    std_errors <- dplyr::bind_rows(!!!ratio_reps) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(std_error = sd(r)) %>% 
        dplyr::ungroup()
    return(std_errors)
}
# auxiliary function, bootstrap samples of the data and computes ratio estimator
sd_ratio_estimation_aux <- function(data, data_stratum, ...){
    sample_boot <- select_sample_prop(data, stratum = strata, frac = 1, 
        replace = TRUE)
    ratio_estimation(data = select(sample_boot, -n_strata), stratum = strata, 
        data_stratum = data_stratum, n_stratum = n_strata, !!!dplyr::quos(...), 
        std_errors = FALSE)

}

