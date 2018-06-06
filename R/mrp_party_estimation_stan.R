#' Postratified estimation for the number of votes for several candidates.
#'
#' The function fits a model using the \code{rstan} package and
#' predicts proportional of votes for all candidates candidate in unobserved polling
#' stations. Optionally the model can be fit with a stratified random sample
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @importFrom rstan sampling
#' @export
mrp_estimation_stan <- function(data, stratum, 
                                n_iter = 350, n_warmup = 150, 
                                n_chains = 1, seed = NA, model_string = NULL,
                                set_strata_na = integer(0)){
    if (is.null(model_string)) {
        model_string <- "neg_binomial_edo"
    }
    # get variables
    stratum_enquo <- dplyr::enquo(stratum)
    #parties <- dplyr::quos(...)
    
    # choose stratum grouping and order of run
    nal_2012 <- get(data(list = "nal_2012", package = "quickcountmx"))
    orden_estados <- nal_2012 %>% dplyr::group_by(region) %>%
        dplyr::summarise(prd = sum(prd_pt_mc)/sum(total)) %>%
        dplyr::arrange(prd)
    estados_1 <- orden_estados$region
    regiones <- dplyr::data_frame(
        area = c(rep(1, 7), rep(2,6), rep(3,6), rep(4,4), rep(5,5), rep(6, 4)),
        region = estados_1)
    data_split <- data %>% 
        dplyr::left_join(regiones, by = c("iD_ESTADO" = "region"))
    data_split$OTROS <- data_split$CNR + data_split$NULOS
    marco_nal_2018 <- get(data(list = "marco_nal_2018", package = "quickcountmx"))
    marco_split <- marco_nal_2018 %>% 
        dplyr::left_join(regiones, by ="region")
    
    partidos <- c("RAC", "JAMK", "AMLO", "JHRC", "OTROS")
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
            dplyr::filter(id %in% data_run$id) ###### 1
        
        df_full <- marco_run %>% 
            dplyr::ungroup() %>% 
            dplyr::select(id, rural, tamano_md, tamano_gd, region) ###### 2
        df_full$region <- factor(df_full$region)
        #x_full <- model.matrix(~-1 + rural+tamano_md+tamano_gd, data = df_full)
        ############### CORREGIR
        df_full$rural[is.na(df_full$rural)] <- 0
        ########
        x_full <- model.matrix(~-1 + rural+tamano_md+tamano_gd+region, data = df_full)
        x <- x_full[df_full$id %in% data_run$id, ]  ####### 3
        print(dim(x))
        print(dim(x_full))
        y <- data_run[, party_name][[1]]
        head(y)
        data_sample <- list(N = nrow(data_run), n = data_run$ln_total,
                            n_covariates = ncol(x),
                            n_strata = length(unique(marco_run$estrato_run)),
                            y = y,
                            stratum = dplyr::pull(data_run, estrato_run),
                            x = x
        )
        #x_full_num <- as.matrix(data %>% ungroup %>% select(rural, tamano_md, tamano_gd, region))
        y_f = marco_run[, party_name][[1]]
        y_f[is.na(y_f)] <- 0
        head(y_f)
        data_full <- list(N_f = nrow(marco_run), n_f = marco_run$ln_total,
                          n_covariates_f = ncol(x),
                          in_sample = as.numeric(marco_run$id %in% data_run$id), #### 4
                          n_strata_f = length(unique(marco_run$estrato_run)),
                          y_f = y_f,
                          stratum_f = dplyr::pull(marco_run, estrato_run),
                          x_f = x_full)
        stan_fit <- rstan::sampling(quickcountmx:::stanmodels[[model_string]], iter = n_iter,
                                    warmup = n_warmup, chains = n_chains, 
                                    data = c(data_sample, data_full),
                                    init = "0", cores = n_chains, control = list(max_treedepth = 12))
        data_frame(y = rstan::extract(stan_fit, 'y_out')[[1]], index = index)
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
        dplyr::summarise(mean_post = 100*mean(prop), 
                         median_post = 100*median(prop),
                         std_dev_post = 100 * sd(prop)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(int_l = pmax(0, mean_post - 1.96 * std_dev_post),
                      int_r = pmin(100, mean_post + 1.96 * std_dev_post)) %>%
        dplyr::arrange(desc(mean_post))
    list(fit = models_party, post_summary = resumen, orden = orden)
}
