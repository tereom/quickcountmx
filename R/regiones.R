#' Regiones
#'
#' A dataset containing a stratification of Mexico's states, this used to 
#' speed up estimations for the national model.
#'
#' @format A data frame with 32 rows and 2 variables:
#' \describe{
#'   \item{area}{geographical area.}
#'   \item{id_estado}{State id}
#' }
#' @source \url{https://cartografia.ife.org.mx}
"regiones"
# library(tidyverse)
# regiones <- data_frame(area = 1:7, 
#     id_estado = list(c(2, 3, 26, 8, 5, 25, 10), 
#         c(19, 28, 24, 32, 1), 
#         c(18, 14, 6, 11, 16),
#         c(15, 22, 13), 
#         c(9, 17),
#         c(12, 21, 29, 30),
#         c(20, 27, 4, 31, 23, 7))) 
# regiones <- marco_nal_2018 %>%
#     count(id_estado) %>%
#         left_join(regiones_long) %>%
#     select(area, id_estado)
