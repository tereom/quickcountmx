#' Presidential electoral results 2006.
#'
#' A dataset containing the election results for president 2006, each row
#' corresponds to a polling station. The data provided by INE lacked some 
#' covariates corresponding to 2006, when this occurs we use the corresponding
#' value in 2017.
#'
#' @format A data frames:
#' @source \url{https://cartografia.ife.org.mx}
"nal_2006"
# library(tidyverse)
# nal_2006_raw <- read_delim(fs::path_join(c("~/Documents/GitHub/ine_cotecora/", 
#     "datos/Computos2006-Presidente__con_EDMS_2017-1.txt")), "|", 
#     escape_double = FALSE, trim_ws = TRUE) %>% 
#     rename(PANAL = X19)
# nal_2006 <- nal_2006_raw %>%
#     dplyr::mutate(
#         casilla_id = 1:n(),
#         edo_id = ID_ESTADO,
#         distrito_fed = DISTRITO,
#         seccion = SECCION,
#         casilla = dplyr::case_when(
#             stringr::str_detect(TIPO_CASILLA, "[B-C]") ~ "B-C",
#             stringr::str_detect(TIPO_CASILLA, "E") ~ "E",
#             stringr::str_detect(TIPO_CASILLA, "S") ~ "S",
#             TRUE ~ "M"
#         ),
#         tipo_seccion = TIPO_SECC_2017,
#         rural = (tipo_seccion == "R") * 1,
#         rural = ifelse(is.na(rural), 0, rural),
#         region = ID_ESTADO,
#         pri_pvem = APM,
#         pan = PAN,
#         panal = PANAL,
#         prd_pt_conv = PBT,
#         psd = ASDC,
#         otros = NO_VOTOS_NULOS + NO_VOTOS_CAN_NREG,
#         total = pri_pvem + pan + panal + prd_pt_conv + otros,
#         ln = LISTA_NOMINAL
#     ) %>%
#     dplyr::group_by(region, seccion) %>%
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
#         casilla_ex = (casilla == "E") * 1,
#         ln_total = ifelse(ln == 0, total, ln),
#         estrato = as.numeric(factor(str_c(ID_ESTADO, DISTRITO, sep = "-")))
#     )  %>%
#     dplyr::select(casilla_id:ln, tamano_md:ln_total, estrato)
