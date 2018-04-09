#' Postratified estimation for the number of votes for a given candidate.
#'
#' The function fits a model using the \code{\link[R2jags]{jags}} function and
#' predicts number of votes for a given candidate in unobserved polling
#' stations.
#' @param data A \code{data.frame} with variables: ln_total, region,
#'   distrito_loc_17, tamano_md, tamano_gd, casilla_ex and the column with
#'   number of votes for the party.
#' @param party unquoted variable indicating the column from the data.frame to
#'   be modeled.
#' @param stratum If sampling the data, unquoted variable indicating the column
#'   from the data.frame to be used as strata. The strata will also be used in
#'   the hierarchical structure of the model.
#' @param frac If sampling the data, numeric value indicating the fraction
#'   of the data to sample, the sample is selected using stratified sampling
#'   with probability proportional to size.
#' @param n_iter,n_chains,n_burnin Number of iterations, chains and burnin size
#'  to be used in \code{\link[R2jags]{jags}}.
#' @param seed Integer value used to set the state of the random number
#'   generator.
#' @return A \code{list} with the object fitted using R2jags::jags and the vector
#'   of simulated counts per candidate.
#' @param modelo_jags optional string specifying variations of the jags model.
#' @details The default model is:
#'   \deqn{X_k \sim N(n_k \theta_k, n_{k}\sigma^2)}
#'   \deqn{
#'   \theta_k=\beta^0 + \beta^{rural}\cdot rural_k + \beta^{tipoSp}\cdot
#'    tipoSp_k + \\ \beta^{tipoEx}\cdot tipoEx_k + \beta^{tamanoMd}\cdot
#'    tamanoMd_k + \beta^{tamanoGd}\cdot tamanoGd_k +
#'    \beta^{distrito}_{distrito(k)} }
#'    \deqn{\beta_{dl}\sim N(\beta_{region(k)}^{region}, \sigma_{dl}^2)}
#'
#' @examples
#' \donotrun{
#' data("gto_2012")
#' mrp_party_estimation(gto_2012, party = pan_na, stratum = distrito_loc_17,
#'   frac = 1, seed = 2212)
#'   }
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
mrp_party_estimation <- function(data, party, stratum, frac = 1,
    n_chains = 3, n_iter = 1000, n_burnin = 500, seed = NA, model_string = NA){
    stratum_enquo <- dplyr::enquo(stratum)
    party_enquo <- dplyr::enquo(party)
    party_name <- dplyr::quo_name(party_enquo)

    if (frac >= 1){
        data_model <- data
    } else {
        data_sample <- select_sample_prop(data, stratum = !!stratum_enquo,
            frac = frac, seed = seed)
        data_model <- data %>%
            dplyr::mutate(!!party_name := ifelse(casilla_id %in%
                data_sample$casilla_id, !!party_enquo, NA))
    }

    data_district <- dplyr::distinct(data, !!stratum_enquo, region) %>%
        dplyr::arrange(!!stratum_enquo)
    data_jags <- list(N = nrow(data_model), n = data_model$ln_total,
        n_regiones = dplyr::n_distinct(data_district$region),
        n_distritos = nrow(data_district),
        x = dplyr::pull(data_model, !!party_enquo),
        rural = data_model$rural,
        estrato = dplyr::pull(data_model, !!stratum_enquo),
        tamano_md = data_model$tamano_md,
        tamano_gd = data_model$tamano_gd, tipo_ex = data_model$casilla_ex,
        region = data_district$region)
    if (is.na(model_string)){
        model_string <-
            "
            model{
                for(k in 1:N){
                    x[k] ~ dnorm(theta[k], tau / n[k]) T(0, 750)
                    theta[k] <- beta_0 + beta_rural * rural[k] +
                    beta_rural_tamano_md * rural[k] * tamano_md[k] +
                    beta_estrato[estrato[k]] + beta_tamano_md * tamano_md[k] +
                    beta_tamano_gd * tamano_gd[k] + beta_tipo_ex * tipo_ex[k]
                }
                beta_0_adj <- beta_0 + mean(beta_estrato[])
                for(j in 1:n_distritos){
                    beta_estrato[j] ~ dnorm(beta_region[region[j]], tau_estrato)
                    beta_estrato_adj[j] <- beta_estrato[j] - mean(beta_estrato[])
                }
                for(j in 1:n_regiones){
                    beta_region[j] ~ dnorm(mu_region, tau_region)
                    beta_region_adj[j] <- beta_region[j] - mean(beta_region[])
                }
                beta_0 ~ dnorm(0, 0.0005)
                beta_rural ~ dnorm(0, 0.0005)
                beta_tamano_md  ~ dnorm(0, 0.0005)
                beta_tamano_gd  ~ dnorm(0, 0.0005)
                beta_tipo_ex  ~ dnorm(0, 0.0005)
                beta_rural_tamano_md  ~ dnorm(0, 0.0005)
                mu_region ~ dnorm(0, 0.0001)
                sigma ~ dunif(0, 50)
                tau <- pow(sigma, -2)
                sigma_estrato ~ dunif(0, 80)
                tau_estrato <- pow(sigma_estrato, -2)
                sigma_region ~ dunif(0, 100)
                tau_region <- pow(sigma_region, -2)
            }
        "
    }
    temp_file <- tempfile(pattern = "model_string", fileext = ".txt")
    cat(model_string, file = temp_file)
    fit_jags <- R2jags::jags(
        # inits = jags_inits,
        data = data_jags,
        parameters.to.save = c("x", "beta_rural", "beta_0", "beta_estrato",
            "beta_tamano_md", "beta_tamano_gd", "beta_tipo_ex", "beta_region",
            "sigma", "sigma_estrato", "beta_rural_tamano_md",
            "beta_0_adj", "sigma_region", "beta_region_adj",
            "beta_estrato_adj"),
        model.file = temp_file,
        n.chains = n_chains,
        n.iter = n_iter,
        n.burnin = n_burnin,
        jags.seed = seed
    )
    n_votes <- apply(fit_jags$BUGSoutput$sims.list$x, 1, sum)
    return(list(fit = fit_jags, n_votes = n_votes))
}
