#' Posterior predictive checks for bayesian vote count models.
#'

#' @param model_fit_party Output of \code{mrp_party_estimation}
#' @param model_fit Output of \code{mrp_estimation}
#'
#' @return A \code{tbl} with posterior simulations for observed
#' polling stations, with observed vote counts
#' @examples
#' data("gto_2012")
#' model <- mrp_party_estimation(gto_2012, party = pan_na, stratum = distrito_loc_17,
#'   frac = 1, seed = 2212)
#' pp_check <- pp_check_party(model)
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @name pp_check
NULL
#> NULL
#' @rdname pp_check
#' @export
pp_check_party <- function(model_fit_party){
    data <- model_fit_party$fit$model$data()
    stratum <- data$estrato
    x_pred <- model_fit_party$fit$BUGSoutput$sims.list$x_pred %>% t
    x_obs <- data$x
    x_pred_df <- dplyr::as_data_frame(x_pred) %>%
        dplyr::mutate(stratum = stratum, observed = x_obs) %>%
        tidyr::gather(n_sim, value, -stratum, -observed) %>%
        filter(!is.na(observed)) %>%
        dplyr::group_by(stratum, n_sim) %>%
        summarise(n_votes_post = sum(value), observed = sum(observed))
    return(x_pred_df)
}

#' @rdname pp_check
#' @export
pp_check <- function(model_fit){
    df_models <- purrr::map(model_fit$jags_fit, function(m_fit){
        data <- m_fit$model$data()
        stratum <- data$estrato
        x_pred <- m_fit$BUGSoutput$sims.list$x_pred %>% t
        x_obs <- data$x
        x_pred_df <- dplyr::as_data_frame(x_pred) %>%
            dplyr::mutate(stratum = stratum, observed = x_obs) %>%
            tidyr::gather(n_sim, value, -stratum, -observed) %>%
            filter(!is.na(observed)) %>%
            dplyr::group_by(stratum, n_sim) %>%
            dplyr::summarise(n_votes_post = sum(value),
                observed = sum(observed))
        x_pred_df
    })
    names(df_models) <- names(model_fit$jags_fit)
    df_all <- dplyr::bind_rows(df_models, .id = "meta_information") %>%
        rename(party = meta_information) %>%
        group_by(stratum, n_sim) %>%
        mutate(prop_post = n_votes_post / sum(n_votes_post),
            prop_obs = observed / sum(observed))
    df_all
}
