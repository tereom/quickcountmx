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
#' @param seed_jags Seed for the call \code{\link[R2jags]{jags}}.
#' @param model_string String with JAGS model, or string indicating the model
#'  to be used, defaults to \code{"model_hier"}, also available
#'  \code{"model_2hier"}, this last one has two nested hierarchies: strata
#'  within regions, and regions modeled form a common distribution.
#' @return A \code{list} with the object fitted using R2jags::jags and the vector
#'   of simulated counts per candidate.
#' @param modelo_jags optional string specifying variations of the jags model.
#' @details The default model is:
#'   \deqn{X_k \sim N(n_k \theta_k, n_{k}\sigma^2)}
#'   \deqn{
#'   \theta_k=\beta^0 + \beta^{rural}\cdot rural_k + \beta^{tipoSp}\cdot
#'    tipoSp_k + \\ \beta^{tipoEx}\cdot tipoEx_k + \beta^{tamanoMd}\cdot
#'    tamanoMd_k + \beta^{tamanoGd}\cdot tamanoGd_k +
#'    \beta^{strata}_{strata(k)}
#'    }
#'    \deqn{\beta_{strata}\sim N(\beta_{region(k)}^{region}, \sigma_{dl}^2)}
#'
#' @examples
#' data("gto_2012")
#' mrp_party_estimation(gto_2012, party = pan_na, stratum = distrito_loc_17,
#'   frac = 1, seed = 2212)
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @export
mrp_party_estimation <- function(data, party, stratum, frac = 1,
    n_chains = 2, n_iter = 1000, n_burnin = 500, seed = NA, seed_jags = NA,
    model_string = "model_bern_t"){
    stratum_enquo <- dplyr::enquo(stratum)
    party_enquo <- dplyr::enquo(party)
    party_name <- dplyr::quo_name(party_enquo)
    if (is.na(seed_jags)) seed_jags <- sample(1:1000, 1)
    if (is.na(seed)) seed <- sample(1:1000, 1)
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
        n_strata = nrow(data_district),
        x = dplyr::pull(data_model, !!party_enquo),
        rural = data_model$rural,
        estrato = dplyr::pull(data_model, !!stratum_enquo),
        tamano_md = data_model$tamano_md,
        tamano_gd = data_model$tamano_gd, tipo_ex = data_model$casilla_ex,
        region = (data_model$region == 1) * 1, # región binaria covariable
        region_mod = data_district$region) # region para modelar (jerárquica)
    if(model_string == "model_2hier"){
        fit_ests <- model_2hier(data_jags, n_chains, n_iter, n_burnin,
            seed_jags)
    } else if (model_string == "model_bern_t"){
        fit_ests <- model_bern_t(data_jags, n_chains, n_iter, n_burnin,
            seed_jags)
    }
    return(fit_ests)
}

model_bern_t <- function(data_jags, n_chains, n_iter, n_burnin, seed_jags){
    model_string <-
        "
    model{
    for(k in 1:N){
    b[k] ~ dbern(p[k])
    p[k] <- ilogit(beta_0_p + beta_rural_p * rural[k] +
        beta_rural_tamano_md_p * rural[k] * tamano_md[k] +
        beta_estrato_raw_p[estrato[k]] + beta_tamano_md_p * tamano_md[k] +
        beta_tamano_gd_p * tamano_gd[k] + beta_tipo_ex_p * tipo_ex[k] +
        beta_region_p * region[k])

    x[k] ~ dt(n[k] * theta[k] * b[k], tau[estrato[k]] / n[k] * (1 / (b[k] + 0.001)), nu[estrato[k]]) T(-0.01, 750)

    theta[k] <- ilogit(beta_0 + beta_rural * rural[k] +
    beta_rural_tamano_md * rural[k] * tamano_md[k] +
    beta_estrato_raw[estrato[k]] + beta_tamano_md * tamano_md[k] +
    beta_tamano_gd * tamano_gd[k] + beta_tipo_ex * tipo_ex[k] +
    beta_region * region[k])
    }
    beta_0_p ~ dnorm(0, 0.25)
    beta_rural_p ~ dnorm(0, 0.25)
    beta_region_p ~ dnorm(0, 0.25)
    beta_tamano_md_p  ~ dnorm(0, 0.25)
    beta_rural_tamano_md_p ~ dnorm(0, 0.25)
    beta_tamano_gd_p  ~ dnorm(0, 0.25)
    beta_tipo_ex_p ~ dnorm(0, 0.25)



    beta_0 ~ dnorm(0, 0.25)
    beta_rural ~ dnorm(0, 0.25)
    beta_region ~ dnorm(0, 0.25)
    beta_tamano_md  ~ dnorm(0, 0.25)
    beta_tamano_gd  ~ dnorm(0, 0.25)
    beta_tipo_ex  ~ dnorm(0, 0.25)
    beta_rural_tamano_md  ~ dnorm(0, 0.25)

    beta_0_adj <- beta_0 + mean(beta_estrato_raw[])
    beta_0_p_adj <- beta_0_p + mean(beta_estrato_raw_p[])

    for(j in 1:n_strata){
      beta_estrato[j] <-  beta_estrato_raw[j] - mean(beta_estrato_raw[])
      beta_estrato_raw[j] ~ dnorm(mu_estrato, tau_estrato)
      beta_estrato_p[j] <-  beta_estrato_raw_p[j] - mean(beta_estrato_raw_p[])
      beta_estrato_raw_p[j] ~ dnorm(mu_estrato_p, tau_estrato_p)
      tau[j] <- pow(sigma[j], -2)
      sigma[j] ~ dunif(0, 10)
      nu[j] ~ rgamma(2, 0.1)
    }
    mu_estrato ~ dnorm(0, 0.25)
    sigma_estrato ~ dunif(0, 25)
    tau_estrato <- pow(sigma_estrato, -2)
    mu_estrato_p ~ dnorm(0, 0.25)
    sigma_estrato_p ~ dunif(0, 0.25)
    tau_estrato_p <- pow(sigma_estrato, -2)
    }
    "

    temp_file <- tempfile(pattern = "model_string", fileext = ".txt")
    cat(model_string, file = temp_file)

    data_jags[["n_regiones"]] <- NULL
    data_jags[["region_mod"]] <- NULL
    data_jags[["b"]] <-  (data_jags[["x"]] > 0) * 1
    fit_jags <- R2jags::jags(
        # inits = jags_inits,
        data = data_jags,
        parameters.to.save = c("x", "beta_0", "beta_0_adj", "beta_rural",
            "beta_tamano_md", "beta_tamano_gd", "beta_tipo_ex",
            "beta_estrato", "beta_estrato_raw",
            "sigma", "sigma_estrato", "beta_rural_tamano_md",
            "beta_region", "beta_0_p", "beta_rural_p", "nu",
            "beta_tamano_md_p", "beta_tamano_gd_p", "beta_tipo_ex_p",
            "beta_estrato_p", "beta_estrato_raw_p", "beta_0_p_adj",
            "sigma_estrato_p",
            "beta_region_p"),
        model.file = temp_file,
        n.chains = n_chains,
        n.iter = n_iter,
        n.burnin = n_burnin,
        jags.seed = seed_jags
    )
    n_votes <- apply(fit_jags$BUGSoutput$sims.list$x, 1, sum)
    return(list(fit = fit_jags, n_votes = n_votes))
}



model_t <- function(data_jags, n_chains, n_iter, n_burnin, seed_jags){
  model_string <-
    "
  model{
  for(k in 1:N){
    x[k] ~ dt(n[k] * theta[k], tau[estrato[k]] / n[k] , nu[estrato[k]]) T(-0.01, 750)
    theta[k] <- ilogit(beta_0 + beta_rural * rural[k] +
    beta_rural_tamano_md * rural[k] * tamano_md[k] +
    beta_estrato_raw[estrato[k]] + beta_tamano_md * tamano_md[k] +
    beta_tamano_gd * tamano_gd[k] + beta_tipo_ex * tipo_ex[k] +
    beta_region * region[k])
  }
  
  beta_0 ~ dnorm(0, 0.25)
  beta_rural ~ dnorm(0, 0.25)
  beta_region ~ dnorm(0, 0.25)
  beta_tamano_md  ~ dnorm(0, 0.25)
  beta_tamano_gd  ~ dnorm(0, 0.25)
  beta_tipo_ex  ~ dnorm(0, 0.25)
  beta_rural_tamano_md  ~ dnorm(0, 0.25)
  
  beta_0_adj <- beta_0 + mean(beta_estrato_raw[])
  beta_0_p_adj <- beta_0_p + mean(beta_estrato_raw_p[])
  
  for(j in 1:n_strata){
    beta_estrato[j] <-  beta_estrato_raw[j] - mean(beta_estrato_raw[])
    beta_estrato_raw[j] ~ dnorm(mu_estrato, tau_estrato)
    beta_estrato_p[j] <-  beta_estrato_raw_p[j] - mean(beta_estrato_raw_p[])
    beta_estrato_raw_p[j] ~ dnorm(mu_estrato_p, tau_estrato_p)
    tau[j] <- pow(sigma[j], -2)
    sigma[j] ~ dunif(0, 10)
    nu[j] ~ rgamma(2, 0.1)
  }

  mu_estrato ~ dnorm(0, 0.25)
  sigma_estrato ~ dunif(0, 25)
  tau_estrato <- pow(sigma_estrato, -2)
  }
  "
  
  temp_file <- tempfile(pattern = "model_string", fileext = ".txt")
  cat(model_string, file = temp_file)
  
  data_jags[["n_regiones"]] <- NULL
  data_jags[["region_mod"]] <- NULL
  fit_jags <- R2jags::jags(
    # inits = jags_inits,
    data = data_jags,
    parameters.to.save = c("x", "beta_0", "beta_0_adj", "beta_rural",
                           "beta_tamano_md", "beta_tamano_gd", "beta_tipo_ex",
                           "beta_estrato", "beta_estrato_raw",
                           "sigma", "sigma_estrato", "beta_rural_tamano_md",
                           "beta_region",  "nu"),
    model.file = temp_file,
    n.chains = n_chains,
    n.iter = n_iter,
    n.burnin = n_burnin,
    jags.seed = seed_jags
  )
  n_votes <- apply(fit_jags$BUGSoutput$sims.list$x, 1, sum)
  return(list(fit = fit_jags, n_votes = n_votes))
}
