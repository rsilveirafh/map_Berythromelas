# Mapa Bothrops erythomelas

# Ricardo da Silveira Filho - ricardodasilveira@gmail.com
# Silvilene Matias - silvilenematias@gmail.com
# 01 dez 2022


# 0) Setup ----------------------------------------------------------------

library(janitor)
library(ggstar)
library(parzer)
library(patchwork)
library(readxl)
library(tidyverse)
library(sf)


# 1) Data -----------------------------------------------------------------

dados <- read_xlsx(path = "data/coords_Berythromelas.xlsx") %>% 
    clean_names() %>% 
    mutate(lat = parse_lat(latitude),
           lon = parse_lon(longitude)) %>%   
    select(-c(latitude, longitude))



# 2) Shapes ---------------------------------------------------------------

# Carregando shape Admnistrativo do BR
bra_adm <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/BRA_adm/")
# plot(bra_adm[5])

# carregando biomas
biomas <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/biomas/", layer = "bioma") %>% 
    st_set_crs(st_crs(bra_adm))
plot(biomas)
cores_biomas <- c("#01665e", "#d8b365", "#f6e8c3", "#8c510a", "#c7eae5", "#5ab4ac")
# ordem: AMAZONIA, CAATINGA, CERRADO, PAMPA, PANTANAL, MATA ATLANTICA


# Carregando shape Mundo
mundo <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/world_high/")
# plot(mundo)


# 3) Plot simples ---------------------------------------------------------

# a <- 
ggplot() +
    geom_sf(data = mundo, size = 0.6, color = "black", fill = "white") +
    geom_sf(data = bra_adm, alpha = 0.2, size = 0.2, color = "black",
            fill = "#6A956D") +
    geom_sf(data = biomas, alpha = 0.4, color = NA,
            fill = cores_biomas) +
    coord_sf(xlim = c(-95, -35), ylim = c(10, -53),
              crs = "EPSG:4674") +
    annotate(geom = "rect", xmin = -33, xmax = -49, ymin = -20, ymax = -2,
             alpha = 0.2, color = "red", fill = NA) +
    theme_minimal() +
    theme(panel.background = element_rect("#edf8fb"),
          axis.text = element_blank())



# b <- 
ggplot() +
    geom_sf(data = mundo, size = 0.6, color = "black", fill = "white") +
    geom_sf(data = bra_adm, alpha = 0.2, size = 0.2, color = "black",
            fill = "#6A956D") +
    geom_sf(data = biomas, alpha = 0.4, color = NA,
            fill = cores_biomas) +
    coord_sf(xlim = c(-30, -49), ylim = c(-20, -2), #
             crs = "EPSG:4674") +
    geom_point(data = dados, aes(x = lon, y = lat), 
               size = dados$total + 2,
               color = ifelse(str_detect(dados$especie, "lutzi"), "#5E262D", "#B21E00"),
               shape = ifelse(dados$usados_na_qualificacao == 0, 1, 16)) +
    geom_star(aes(x = -40.509070, y = -9.425430), size = 3,
              fill = "red") +
    labs(x = "Longitude", 
         y = "Latitude") +
    theme_minimal() +
    theme(panel.background = element_rect("#edf8fb"))




b + inset_element(a, left = 0.3, bottom = 0.034, right = 1.009, top = 0.3, align_to = "full")














