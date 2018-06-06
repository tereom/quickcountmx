#' Postratified estimation for the number of votes for a given candidate.
#'
#' The function fits a model using the \code{rstan} package and
#' predicts number of votes for a given candidate in unobserved polling
#' stations. Optionally the model can be fit with a stratified random sample
#' of the data, and predict for the population.
#' @param data A \code{data.frame} with variables: ln_total, region,
#'   distrito_loc_17, tamano_md, tamano_gd, casilla_ex and the column with
#'   number of votes for the party.
#' @param party Unquoted variable indicating the column from the data.frame to
#'   be modeled.
#' @param stratum If sampling the data, unquoted variable indicating the column
#'   from the data.frame to be used as strata. The strata will also be used in
#'   the hierarchical structure of the model.
#' @param frac If sampling the data, numeric value indicating the fraction
#'   of the data to sample, the sample is selected using stratified sampling
#'   with probability proportional to size.
#' @param n_iter,warmup,n_chains Number of iterations, number of warmup
#'   iterations and number of chains.
#'  to be used in \code{\link[rstan]{sampling}}.
#' @param seed Integer value used to set the state of the random number
#'   generator.
#' @param model_string String indicating the model to be used, defaults to
#'   \code{"neg_binomial"}.
#' @param set_strata_na Option to exclude strata when fitting the model, used
#'   for model evaluation and calibration.
#' @return A list containing fit object and vector
#'   the simulated counts for the candidate.
#' @examples
#' mrp_party_pri <- mrp_party_estimation_stan(nal_2012, pri_pvem, frac = 0.02,
#'     stratum = estrato, n_iter = 300, warmup = 150,
#'     n_chains = 2, seed = 19291)
#' quantile(mrp_party$y)
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @importFrom rstan sampling
#' @export
mrp_party_estimation_stan <- function(data, party, stratum, frac = 1,
                                      n_iter = 300, warmup = 150, n_chains = 2, seed = NA, model_string = NULL,
                                      set_strata_na = integer(0)){
    if (is.null(model_string)) {
        model_string <- "neg_binomial"
    }
    stratum_enquo <- dplyr::enquo(stratum)
    party_enquo <- dplyr::enquo(party)
    if (is.na(seed)) seed <- sample(1:1000, 1)
    data_sample <- select_sample_prop(data, stratum = !!stratum_enquo,
                                      frac = frac, seed = seed)
    data_district <- dplyr::distinct(data, !!stratum_enquo, region) %>%
        dplyr::arrange(!!stratum_enquo)
    x <- dplyr::select(data_sample %>% dplyr::ungroup(),
                       tamano_md, tamano_gd, rural) %>% as.matrix
    stratum_vec <- dplyr::pull(data_sample, !!stratum_enquo)
    if (length(set_strata_na) > 0) {
        x[stratum_vec %in% set_strata_na] <- NA
    }
    data_sample <- list(N = nrow(data_sample), n = data_sample$ln_total,
                        n_covariates = ncol(x),
                        n_strata = length(unique(data_sample %>% dplyr::pull(!!stratum_enquo))),
                        y = dplyr::pull(data_sample, !!party_enquo),
                        stratum = dplyr::pull(data_sample, !!stratum_enquo),
                        x = x)
    x_full <- dplyr::select(data %>% dplyr::ungroup(),
                            tamano_md, tamano_gd, rural) %>% as.matrix
    data_full <- list(N_f = nrow(data), n_f = data$ln_total,
                      n_covariates_f = ncol(x_full),
                      in_sample = as.numeric(data$casilla_id %in% data_sample$casilla_id),
                      n_strata_f = nrow(data_district),
                      y_f = dplyr::pull(data, !!party_enquo),
                      stratum_f = dplyr::pull(data, !!stratum_enquo),
                      x_f = x_full)
    stan_fit <- sampling(stanmodels[[model_string]], iter = n_iter,
                         warmup = warmup, chains = n_chains, data = c(data_sample, data_full),
                         init = "0", cores = n_chains, control = list(max_treedepth = 15))
    y_sims <- rstan::extract(stan_fit, "y_out")[[1]]
    return(list(fit = stan_fit, y = y_sims))
}
