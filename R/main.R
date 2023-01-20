# Mapa Bothrops erythomelas

# Ricardo da Silveira Filho - ricardodasilveira@gmail.com - 01 dez 2020
# Silvilene Matias - silvilenematias@gmail.com - 01 dez 2020


# 0) Setup ----------------------------------------------------------------

# Pacotes
library(janitor)
library(ggspatial)
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
           lon = parse_lon(longitude),
           qualif = ifelse(usados_na_qualificacao == 0, "não", "sim")) %>%   
    select(-c(latitude, longitude))


# 2) Shapes ---------------------------------------------------------------

# Carregando shape Admnistrativo do BR
bra_adm <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/BRA_adm/")
# plot(bra_adm[5])


# Carregando biomas
biomas <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/biomas/", layer = "bioma") %>% 
    st_set_crs(st_crs(bra_adm))
# plot(biomas)

# Cores dos biomas
# ordem: AMAZONIA, CAATINGA, CERRADO, PAMPA, PANTANAL, MATA ATLANTICA
cores_biomas <- c("#01665e", "#d8b365", "#f6e8c3", "#8c510a", "#c7eae5", "#5ab4ac")


# Carregando shape Mundo
mundo <- sf::st_read(dsn = "/media/ricardo/backup/maps/shapes/world_high/")
# plot(mundo)


# Carregando Rio S Francisco
rsf <- sf::st_read("/media/ricardo/backup/maps/shapes/RSFrancisco/") %>% 
    filter(Name == "Rio São Francisco")
# plot(rsf[1])


# 3) Plot ----------------------------------------------------------------

# mapa geral
a <- ggplot() +
    geom_sf(data = mundo, size = 0.6, color = "black", fill = "white") +
    geom_sf(data = bra_adm, alpha = 0.2, size = 0.2, color = "black",
            fill = "#6A956D") +
    geom_sf(data = biomas, alpha = 0.4, color = NA,
            fill = cores_biomas) +
    geom_sf(data = rsf, color = "dodgerblue2", alpha = 0.4, linewidth = 1) +
    coord_sf(xlim = c(-95, -35), ylim = c(10, -53),
              crs = "EPSG:4674") +
    annotate(geom = "rect", xmin = -33, xmax = -50, ymin = -20, ymax = -2,
             alpha = 0.2, color = "red", fill = NA) +
    theme_minimal() +
    theme(panel.background = element_rect("#edf8fb"),
          axis.text = element_blank())


# Mapa com zoom
b <- ggplot() +
    geom_sf(data = mundo, size = 0.6, color = "black", fill = "white") +
    geom_sf(data = bra_adm, alpha = 0.2, size = 0.2, color = "black",
            fill = "#6A956D") +
    geom_sf(data = biomas, alpha = 0.4, color = NA,
            fill = cores_biomas) +
    geom_sf(data = rsf, color = "dodgerblue2", alpha = 0.4, linewidth = 2) +
    coord_sf(xlim = c(-30, -49), ylim = c(-20, -2), 
             crs = "EPSG:4674") +
    geom_point(data = dados, aes(x = lon, y = lat,
                                 color = especie,
                                 size = total,
                                 shape = qualif)) +
    scale_color_manual(name = "Espécie",
                       values = c("#de2d26", "#980043"),
                       labels = c(expression(italic("B. erythromelas")), 
                                  expression(italic("B. lutzi")))) +
    scale_size(name = "Quantidade de amostras",
               breaks = c(1, 3, 5),
               range = c(1, 10)) +
    scale_shape_manual(name = "Utilizada na qualificação",
                values = c(16, 1),
                labels = c("Sim", "Não")) +
    geom_star(aes(x = -40.509070, y = -9.425430), size = 3, fill = "red") +
    annotation_scale(location = "tr",
                     bar_cols = c("grey60", "white")) +
    annotation_north_arrow(location = "tr", 
                                      which_north = "true",
                                      pad_x = unit(0.95, "in"), 
                                      pad_y = unit(0.6, "in"),
                                      style = ggspatial::north_arrow_nautical(
                                          fill = c("grey40", "white"),
                                          line_col = "grey20")) +
    annotate(geom = "rect", xmin = -32.3, xmax = -33, ymin = -12.6, ymax = -12.4,
             alpha = 0.6, fill = "dodgerblue2") +
    annotate(geom = "text", x = -30.85, y = -12.5,
             label = "Rio São Francisco") +
    geom_star(aes(x = -33.25, y = -13.17), 
              size = 3, fill = "red") +
    annotate(geom = "text", x = -31.25, y = -13.2, 
             label = expression(paste("Holótipo ", italic("B. erythomelas")))) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(size = 4)),
           shape = guide_legend(override.aes = list(size = 4))) +
    theme(panel.background = element_rect("#edf8fb"),
          legend.position = "right",
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15),
          # legend.text = element_text(size = 13),
          legend.title = element_text(size = 15),
          legend.text.align = 0)


# Unindo os dois mapas
b + inset_element(a, left = 0.5, bottom = 0.05, right = 0.855, top = 0.4, align_to = "full")

# Salvar em *.png
ggsave("output/map_Berythomelas.png",
       width = 15,
       height = 15,
       device = "png")

# Salvar em formato vetorial (*.eps)
# ggsave("output/map_Berythomelas.svg",
#        width = 15,
#        height = 15,
#        device = "svg")








