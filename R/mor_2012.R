# morelos_2012 <- read_delim("DataMorelos/CVE17_Morelos_2012.tsv", 
#     "|", escape_double = FALSE, trim_ws = TRUE)
# 
# glimpse(morelos_2012)
# 
# plot_missing(morelos_2012)
# 
# mor_2012 <- morelos_2012 %>% 
#     mutate(
#         casilla_id = 1:n(),
#         distrito_fed_17 = DISTRITO_FEDERAL_2017,
#         distrito_fed_12 = DISTRITO_FEDERAL_2012,
#         distrito_loc_17 = DISTRITO_LOCAL_2017,
#         distrito_loc_12 = DISTRITO_LOCAL_2012,
#         are = stringr::str_c(distrito_fed_12,
#             ID_AREA_RESPONSABILIDAD_2E_2012, sep = "-"),
#         seccion = SECCION,
#         casilla = dplyr::case_when(
#             stringr::str_detect(TIPO_DE_CASILLA, "[B-C]") ~ "B-C",
#             stringr::str_detect(TIPO_DE_CASILLA, "G") ~ "G",
#             stringr::str_detect(TIPO_DE_CASILLA, "S") ~ "S"
#         ),
#         tipo_seccion = TIPO_SECCION_21ago_2017,
#         pri_pvem_pna = PRI + PVEM + PNA + VCC_PRI_PVEM_PNA,
#         pan = PAN,
#         prd_pt_mc = PRD + PT + MC + VCC_PRD_PT_MC, 
#         psd = PSD, 
#         otros = CANDIDATOS_NO_REGISTRADOS + VOTOS_NULOS,
#         total = pri_pvem_pna + pan + prd_pt_mc + psd + otros,
#         ln = LISTA_NOMINAL
#     ) %>%
#     dplyr::group_by(seccion) %>%
#     dplyr::mutate(ln_seccion = sum(ln)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#         tamano = dplyr::case_when(
#             ln_seccion < 1000 ~ 1,
#             ln_seccion < 5000 ~ 2,
#             TRUE ~ 3
#         ),
#         tamano_md = (tamano == 2) * 1,
#         tamano_gd = (tamano == 3) * 1,
#         region = dplyr::case_when(
#             distrito_loc_17 %in% c(1:6) ~ 1,
#             TRUE ~ 2
#         ),
#         casilla_ex = (casilla == "G") * 1,
#         rural = dplyr::case_when(tipo_seccion == "R" ~ 1, TRUE ~ 0),
#         ln_total = ifelse(ln == 0, total, ln)
#     )  %>%
#     dplyr::select(casilla_id:ln, tamano_md:ln_total) 
# 
# clust <-  parallel::makeCluster(getOption("cl.cores", no_cores))
# system.time({
#     a <- mrp_estimation(mor_2012, pri_pvem_pna:otros, stratum = distrito_loc_17,
#         frac = 0.087, n_iter = 3000, n_burnin = 1500, n_chains = 2, seed = 888, 
#         parallel = TRUE, clust = clust)
#     
# })
# parallel::stopCluster(clust)
