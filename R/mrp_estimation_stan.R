#' Postratified estimation for the number of votes for several candidates.
#'
#' The function fits a model using the \code{rstan} package and
#' predicts proportional of votes for all candidates candidate in unobserved polling
#' stations. Optionally the model can be fit with a stratified random sample
#'
#' @inheritParams mrp_estimation
#' @param n_warmup burnin size
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @importFrom rstan sampling
#' @export
mrp_estimation_stan <- function(data, stratum, 
    n_iter = 500, n_warmup = 250, 
    n_chains = 1, seed = NA, model_string = NULL,
    set_strata_na = integer(0), 
    partidos = c("RAC", "JAMK", "AMLO", "JHRC", "OTROS"),
    frame_name = "marco_nal_2018"){
    if (is.null(model_string)) {
        model_string <- "neg_binomial_edo"
    }
    # get variables
    stratum_enquo <- dplyr::enquo(stratum)
    #parties <- dplyr::quos(...)
    # choose stratum grouping and order of run
    regiones <- get(data(list = "regiones", package = "quickcountmx"))
    if("edo_id" %in% names(data)){
        data$iD_ESTADO <- data$edo_id
    }
    data_split <- data %>% 
        dplyr::left_join(regiones, by = c("iD_ESTADO" = "id_estado"))
    otros_name <- partidos[length(partidos)]
    if("CNR" %in% names(data_split)){
        data_split[[otros_name]] <- data_split$CNR + data_split$NULOS
    }
    marco_nal <- get(data(list = frame_name, package = "quickcountmx"))
    if("edo_id" %in% names(marco_nal)){
        marco_nal$id_estado <- marco_nal$edo_id   
     }
    #########################
    marco_split <- marco_nal %>% 
        dplyr::left_join(regiones, by ="id_estado")
    
    orden <- expand.grid(partidos = partidos, areas = unique(regiones$area),
        stringsAsFactors = FALSE)
    orden <- orden %>% dplyr::as_tibble() %>% dplyr::mutate(index = 1:n())
    
    
    # prepare cluster
    seed <- 35
    clust <-  parallel::makeCluster(getOption("cl.cores", 35))
    parallel::clusterSetRNGStream(clust, seed)
    parallel::clusterExport(clust, c("data_split", "marco_split", 
        "partidos", "orden", "model_string", "n_chains",
        "n_iter", "n_warmup"), envir = environment())
    parallel::clusterApply(clust, seq_along(clust), function(i) {
        zz <<- file(sprintf('all-%d.Rout', i), open='wt')
        sink(zz)
        sink(zz, type='message')
    })
    
    parallel::clusterEvalQ(clust, {
        library(dplyr)
        library(quickcountmx)
        library(rstan)
    })
    
    models_party <- parallel::parLapply(clust, 1:nrow(orden), function(i){
        #models_party <- lapply(1:nrow(orden), function(i){
        index <- orden$index[i]
        party_name <- orden$partidos[i]
        area_name <- orden$areas[i]
        print(party_name)
        print(paste0("Area : ", area_name))
        data_run <- data_split %>% dplyr::filter(area == area_name)
        marco_run <- marco_split %>% dplyr::filter(area == area_name) %>%
            dplyr::mutate(estrato_run = as.numeric(factor(estrato)))
        marco_run <- marco_run %>% 
            dplyr::left_join(data_split)
        data_run <- marco_run %>% 
            dplyr::filter(id %in% data_run$id) 
        
        df_full <- marco_run %>% 
            dplyr::ungroup() %>% 
            dplyr::select(id, rural, tamano_md, tamano_gd, region) 
        df_full$region <- factor(df_full$region)
        x_full <- model.matrix(~-1 + rural+tamano_md+tamano_gd+region, data = df_full)
        x <- x_full[df_full$id %in% data_run$id, , drop = FALSE]  
        print(dim(x))
        print(dim(x_full))
        y <- data_run[, party_name][[1]]
        head(y)
        data_sample <- list(N = nrow(data_run), n = as.array(data_run$ln_total),
            n_covariates = ncol(x),
            n_strata = length(unique(marco_run$estrato_run)),
            y = as.array(y),
            stratum = as.array(dplyr::pull(data_run, estrato_run)),
            x = x
        )
        
        y_f = marco_run[, party_name][[1]]
        y_f[is.na(y_f)] <- 0
        head(y_f)
        data_full <- list(N_f = nrow(marco_run), n_f = marco_run$ln_total,
            n_covariates_f = ncol(x),
            in_sample = as.numeric(marco_run$id %in% data_run$id), 
            n_strata_f = length(unique(marco_run$estrato_run)),
            y_f = y_f,
            stratum_f = dplyr::pull(marco_run, estrato_run),
            x_f = x_full)
        stan_fit <- rstan::sampling(quickcountmx:::stanmodels[[model_string]], iter = n_iter,
            warmup = n_warmup, chains = n_chains, 
            data = c(data_sample, data_full),
            init = "0", cores = n_chains, 
            control = list(max_treedepth = 12, adapt_delta = 0.85))
        dplyr::data_frame(y = rstan::extract(stan_fit, 'y_out')[[1]], index = index)
    })
    
    parallel::clusterEvalQ(clust, {
        sink(type='message')
        sink()
        close(zz)
        rm(zz)
    })
    
    parallel::stopCluster(clust)
    
    df_2 <- dplyr::left_join(dplyr::bind_rows(models_party), orden)
    df_2 <- df_2 %>%  
        dplyr::group_by(partidos, areas) %>%
        dplyr::mutate(num = 1:n()) %>% 
        dplyr::group_by(partidos, num) %>%
        dplyr::summarise(votes=sum(y)) %>% 
        dplyr::group_by(num) %>%
        dplyr::mutate(total = sum(votes), prop = votes/sum(votes)) %>% 
        dplyr::rename(party = partidos)
    part_1 <- df_2 %>% dplyr::group_by(num) %>% 
        dplyr::summarise(votes = sum(votes)) %>%
        dplyr::mutate(party = "participacion") %>%
        dplyr::mutate(total = sum(marco_nal_2018$ln)) %>%
        dplyr::mutate(prop =  votes/total)
    
    resumen <- df_2 %>% 
        dplyr::bind_rows(part_1) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(mean_post = pmin(100, 100*mean(prop)), 
            median_post = pmin(100, 100*median(prop)),
            std_dev_post = 100 * sd(prop)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            int_l = pmin(100, pmax(0, mean_post - 2 * std_dev_post)),
            int_r = pmin(100, mean_post + 2 * std_dev_post)) %>%
        dplyr::arrange(desc(mean_post))
    list(fit = models_party, post_summary = resumen, orden = orden)
}
