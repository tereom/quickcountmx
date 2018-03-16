#' Select simple and stratified random samples from a sampling frame.
#'
#' Select random samples of a sampling frame (a data.frame).
#' \code{select_sample_prop} can be used when sampling with equal probability
#'   across strata or when selecting a simple random sample.
#' \code{select_sample_str} selects samples when sample size varies across
#'   strata.
#' @param sampling_frame \code{data.frame} with the sampling frame it must contain a
#'   column with the stratum.
#' @param allocation \code{data.frame} with a column defining the strata and a
#'   column with sample size allocations for each stratum (one line per stratum).
#' @param sample_size unquoted column with sample sizes in the allocation
#'   data.frame
#' @param stratum unquoted column with strata in the allocation and
#'   sampling_frame \code{data.frame}'s (the columns must have the same name in the
#'   two \code{data.frame}'s). If one wants to select a SRS the stratum
#'   parameter is not used.
#' @param is_frac logical value indicating whether the allocation data.frame contains
#'   proportions to be sampled within each stratum (TRUE) or sample sizes.
#' @param frac when sampling with equal probability across strata, frac is a
#'   numeric value indicating the fraction of the data to select.
#' @return A \code{data.frame} with the selected sample, it will have the same
#'   columns as the original sampling frame plus a column indicating the sample
#'   size in the stratum of each selected observation.
#' @examples
#' sampling_frame <- data.frame(id = 1:100,
#'   str = sample(1:5, 100, replace = TRUE),
#'   val = rnorm(100))
#' allo <- sampling_frame %>%
#'     group_by(str) %>%
#'     summarise(n = 0.4 * n())
#' select_sample_str(sampling_frame, allo, n, str)
#' # SRS (simple random sample)
#' select_sample_prop(sampling_frame, frac = 0.2)
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @name select_sample
NULL
#> NULL

#' @rdname select_sample
#' @export
select_sample_str <- function(sampling_frame, allocation,
    sample_size = sample_size, stratum = stratum, is_frac = FALSE){

    sample_size <- dplyr::enquo(sample_size)
    sample_size_name <- dplyr::quo_name(sample_size)

    stratum_var_string <- deparse(substitute(stratum))
    stratum <- dplyr::enquo(stratum)

    if (is_frac) {
        sample <- sampling_frame %>%
            dplyr::left_join(allocation, by = stratum_var_string) %>%
            split(.[stratum_var_string]) %>%
            purrr::map_df(~dplyr::sample_frac(.,
                size = dplyr::pull(., sample_size_name)[1])) %>%
            dplyr::select(dplyr::one_of(colnames(sampling_frame)))
    } else {
        # if sample size not integer we round it
        allocation <- allocation %>%
            dplyr::mutate(!!sample_size_name := round(!!sample_size)) # %>%
        #select(!!!stratum, sample_size)

        sample <- sampling_frame %>%
            dplyr::left_join(allocation, by = stratum_var_string) %>%
            split(.[stratum_var_string]) %>%
            purrr::map_df(~dplyr::sample_n(.,
                size = dplyr::pull(., sample_size_name)[1])) %>%
            dplyr::select(dplyr::one_of(colnames(sampling_frame)))
    }
    return(sample)
}

#' @rdname select_sample
#' @export
select_sample_prop <- function(sampling_frame, stratum = stratum, frac){
    if(missing(stratum)){
        sample <- dplyr::sample_frac(sampling_frame, size = frac)
    } else {
        stratum_var_string <- deparse(substitute(stratum))
        stratum <- dplyr::enquo(stratum)
        sample <- sampling_frame %>%
            dplyr::group_by(!!stratum) %>%
            dplyr::sample_frac(size = frac)
    }
    return(sample)
}
