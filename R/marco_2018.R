#' Sampling frame for elections 2018.
#'
#' Datasets containing the 2018 sampling frame for Mexican national election, 
#' and sampling frames for Governor elections in Chiapas, Guanajuato, and 
#' Morelos. Each row corresponds to a polling station and the variables include 
#' strata, nominal list, and other descriptorss of the polling station.
#'
#' @format A data frames:
#' @source \url{https://cartografia.ife.org.mx}
"marco_nal_2018"
"marco_chis_2018"
"marco_gto_2018"
"marco_mor_2018"
# library(tidyverse)
# # marco al 20 de junio
marco_erwin <- readr::read_csv(fs::path_join(c("~/Documents/GitHub/",
    "ine_cotecora/datos/LISTADO_CASILLAS_20_JUNIO_DEOE.csv")))

marco_erwin <- readr::read_csv(fs::path_join(c("~/Dropbox/COTECORA 2017-2018/",
    "SIMULACRO4/LISTADO_CASILLAS_2018.csv")))

marco_nal_2018_aux <- marco_erwin

marco_nal_2018 <- marco_nal_2018_aux %>%
    mutate(
        id = stringr::str_c(iD_ESTADO, SECCION, ID_CASILLA, TIPO_CASILLA,
            EXT_CONTIGUA, sep = "-"),
        distrito_loc = ID_DIST_LOC, distrito_fed = ID_DISTRITO_FEDERAL) %>%
    dplyr::mutate(
        casilla_id = 1:n(),
        id_estado = iD_ESTADO,
        seccion = SECCION,
        tipo_seccion = TIPO_SECCION,
        estrato = ID_ESTRATO_F,
        estrato_l = ID_ESTRATO_L,
        casilla = dplyr::case_when(
            stringr::str_detect(TIPO_CASILLA, "[B-C]") ~ "B-C",
            stringr::str_detect(TIPO_CASILLA, "E") ~ "E",
            stringr::str_detect(TIPO_CASILLA, "S") ~ "S",
            TRUE ~ "M"
        ),
        ln = LISTA_NOMINAL
    ) %>%
    dplyr::group_by(id_estado, seccion) %>%
    dplyr::mutate(ln_seccion = sum(ln)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        tamano = dplyr::case_when(
            ln_seccion < 1000 ~ 1,
            ln_seccion < 5000 ~ 2,
            TRUE ~ 3
        ),
        tamano_md = (tamano == 2) * 1,
        tamano_gd = (tamano == 3) * 1,
        region = id_estado,
        casilla_ex = (casilla == "E") * 1,
        rural = (tipo_seccion == 3) * 1,
        ln_total = ifelse(ln == 0, 750, ln)
    ) %>%
    dplyr::select(id, casilla_id, id_estado, distrito_loc, distrito_fed,
        seccion:ln_total) %>%
    dplyr::filter(!is.na(rural))

marco_gto_2018 <- marco_nal_2018 %>%
    filter(id_estado == 11) %>%
    mutate(
        estrato = estrato_l,
        region = dplyr::case_when(
            distrito_loc %in% c(1, 3:8, 10:13, 18, 21) ~ 1,
            TRUE ~ 2
            )
        ) %>%
    select(-estrato_l)

marco_mor_2018 <- marco_nal_2018 %>%
    filter(id_estado == 17)%>%
    mutate(
        estrato = estrato_l,
        region = dplyr::case_when(
            distrito_loc %in% 1:6 ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(-estrato_l)

marco_chis_2018 <- marco_nal_2018 %>%
    filter(id_estado == 7) %>%
    mutate(
        estrato = estrato_l,
        region = dplyr::case_when(
            distrito_loc %in% c(4, 6, 7, 8, 9, 10, 17, 19, 20, 21, 22, 24) ~ 1,
            TRUE ~ 2
        )
    ) %>%
    select(-estrato_l)
