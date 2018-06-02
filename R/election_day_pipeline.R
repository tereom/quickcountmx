#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' @import ggplot2
#'
table_frame <- dplyr::data_frame(estado = c("00","07", "11", "17"),
    marco = c("marco_nal_2018", "marco_chis_2018",
        "marco_gto_2018", "marco_mor_2018"),
    candidatos =
        list(c("RAC", "JAMK", "AMLO", "JHRC"),
            c("JAAB", "RAAG", "RCEC", "LFCC", "JAOR"),
            c("DSRV", "GSG", "FACG", "FRSP", "MBSL"),
            c("VMCS", "JAMO", "MRGC", "NLMLC", "CBB", "JAVJ", "MRA", "FDH")), 
    partidos = 
        list(c("PAN_PRD_MC", "PRI_PVEM_PANAL", "PT_MORENA_PES", "JHRC"),
            c("PAN_PRD_MC", "PRI_PNA", "PT_MORENA_PES", "PVEM_PCU_PMC", 
                "JAOR"),
            c("PAN_PRD_MC", "PRI", "PVEM", "PT_MORENA_PES", "PANAL"),
            c("PAN_MC", "PRI", "PRD_PSD", "PVEM", "PT_MORENA_PES", "PANAL", 
                "PH", "FDH"))
    )

write_results <- function(post_summary, file_name, team, table_frame_in, 
    path_out){
    EN <- stringr::str_sub(file_name, 10, 11)
    R <- stringr::str_sub(file_name, 12, 17)
    cod_party_candidatos <- table_frame_in %>% 
        tidyr::unnest() %>% 
        dplyr::select(candidatos, partidos) %>% 
        dplyr::bind_rows(dplyr::data_frame(candidatos = "PART", 
            partidos = "PART")) 
    tab_base <- post_summary %>% 
        dplyr::mutate(party = ifelse(party == "participacion", "PART",
            party)) %>% 
        dplyr::left_join(cod_party_candidatos, 
            by = c("party" = "candidatos")) %>% 
        dplyr::filter(party != "OTROS") %>% 
        dplyr::rename(candidatos = party) %>% 
        dplyr::select(-mean_post, -std_dev_post)
        
    tab_partidos <- tab_base %>% dplyr::select(-candidatos) %>% 
        tidyr::gather(LMU, value, -partidos) %>% 
        dplyr::mutate(
            EQ = team, 
            EN = EN, 
            R = R, 
            value = round(value, 2),
            LMU = dplyr::case_when(
                LMU == "int_l" ~ 0, 
                LMU == "median_post" ~ 1,
                LMU == "int_r" ~ 2
            ), 
            LMU = as.integer(LMU)
            ) %>% 
        tidyr::spread(partidos, value) %>% 
        dplyr::arrange(LMU) %>% 
        dplyr::select(EQ, EN, R, cod_party_candidatos$partidos, LMU)
        
    tab_candidatos <- tab_base %>% 
        dplyr::select(-partidos) %>% 
        tidyr::gather(LMU, value, -candidatos) %>% 
        dplyr::mutate(
            EQ = team, 
            EN = EN, 
            R = R, 
            value = round(value, 2),
            LMU = dplyr::case_when(
                LMU == "int_l" ~ 0, 
                LMU == "median_post" ~ 1,
                LMU == "int_r" ~ 2
            ), 
            LMU = as.integer(LMU)
        ) %>% 
        tidyr::spread(candidatos, value) %>% 
        dplyr::arrange(LMU) %>% 
        dplyr::select(EQ, EN, R, cod_party_candidatos$candidatos, LMU)
        
    readr::write_csv(tab_partidos, path = paste0(path_out, "/", team, 
        EN, R, ".csv"))
    readr::write_csv(tab_candidatos, path = paste0(path_out, , "/", 
        team, EN, R, "_v2.csv"))
}

#' @export
process_batch <- function(path_name, file_name, path_out, team = "default"){
    print(team)
    all_data_filename = paste0(path_out, "/remesas.rds")
    new_name <- paste0(path_out, "/procesado_", file_name, ".rds")
    data_in <- readr::read_delim(path_name, "|", escape_double = FALSE,
        trim_ws = TRUE)
    print(paste0("datos: ", path_name))
    print(paste0("salidas: ", path_out))
    # do processing ########
    tipo <- stringr::str_sub(file_name, 8, 9)
    estado_str <- stringr::str_sub(file_name, 10, 11)
    table_frame_in <- dplyr::filter(table_frame, estado == estado_str)
    marco_name <- table_frame_in %>% dplyr::pull(marco)
    candidatos <- c(table_frame_in$candidatos[[1]], "OTROS")
    marco <- get(data(list = marco_name, package = "quickcountmx"))
    # get id
    #print(head(data_in))
    data_out <- data_in %>% dplyr::mutate(id =
            stringr::str_c(ID_ESTADO, SECCION, ID_CASILLA, TIPO_CASILLA,
                EXT_CONTIGUA, sep = "-")) %>%
        dplyr::mutate(OTROS = NULOS + CNR) %>%
        dplyr::select(id, dplyr::one_of(candidatos)) %>%
        dplyr::right_join(marco)
    
    #######################
    saveRDS(data_out, file = new_name)
    
    # run model ###################
    if(estado_str != "00") {
        fit_time <- system.time(
            fit <- mrp_estimation(data_out, !!!rlang::syms(candidatos),
                stratum = estrato, n_iter = 1500,
                n_burnin = 500, n_chains = 1,
                mc_cores = length(candidatos), parallel = TRUE)
        )
    }
    print(fit_time)
    print(fit$post_summary)
    #saveRDS(fit$jags_fit, file = paste0("./procesados/fit_", file_name, ".rds"))
    df_new <- dplyr::data_frame(archivo = file_name, hora = Sys.time(),
        datos = list(data_out), originales = list(data_in),
        post_summary = list(fit$post_summary))
    if(file.exists(all_data_filename)){
        df_prev <- readRDS(all_data_filename)
        df_agg <- dplyr::bind_rows(df_prev, df_new)
    } else {
        df_agg <- df_new
    }
    saveRDS(df_agg, all_data_filename)
    # copy results
    output <- fit$post_summary
    #readr::write_csv()
    # extract deviance and counts simulations
    log_like_sims <- lapply(1:length(fit$jags_fit), function(i){
        fit$jags_fit[[i]]$BUGSoutput$sims.list$deviance
    })
    names(log_like_sims) <- names(fit$jags_fit)
    df_ll <- as.data.frame(log_like_sims)
    df_ll$no_sim <- 1:nrow(df_ll)
    df_ll_long <- df_ll %>% tidyr::gather(partido, loglike, -no_sim)
    # vote counts
    counts_sims <- lapply(1:length(fit$jags_fit), function(i){
        sims_1 <- apply(fit$jags_fit[[i]]$BUGSoutput$sims.list$x, 1, sum)
        sims_1
    })
    names(counts_sims) <- names(fit$jags_fit)
    df_cts <- as.data.frame(counts_sims)
    df_cts$no_sim <- 1:nrow(df_cts)
    df_cts_long <- df_cts %>% tidyr::gather(partido, conteo_sim, -no_sim)
    
    # graphs
    ggplot2::ggplot(df_ll_long, ggplot2::aes(x=no_sim, y = loglike)) +
        ggplot2::geom_line(colour = "salmon") +
        ggplot2::facet_wrap(~partido, ncol=1, scales = "free_y")+
        ggplot2::theme_bw() + 
        ggplot2::labs(title = "Simulaciones MCMC de devianza")
    ggplot2::ggsave(paste0(path_out,"/deviance-", file_name, ".png"))
    gr_cts <- ggplot2::ggplot(df_cts_long, ggplot2::aes(x=no_sim, 
        y = conteo_sim, group=partido,
        colour=partido)) +
        ggplot2::geom_line(colour = "salmon") +
        ggplot2::theme_bw() + ggplot2::labs(title = "Simulaciones MCMC de conteos totales") +
        ggplot2::scale_y_log10()
    ggplot2::ggsave(paste0(path_out,"/counts-", file_name, ".png"))
    write_results(post_summary = fit$post_summary, file_name = file_name, 
        team = team, table_frame_in = table_frame_in, path_out = path_out)
}
