# Mapa Bothrops erythomelas

# Ricardo da Silveira Filho - ricardodasilveira@gmail.com
# Silvilene Matias - silvilenematias@gmail.com
# 01 dez 2022


# 0) Setup ----------------------------------------------------------------

library(tidyverse)
library(sf)



# 1) Data -----------------------------------------------------------------

data <- read_csv("data/coord_bothrops.csv")


# 2) Shapes ---------------------------------------------------------------

# Carregando shape Admnistrativo do BR
bra_adm <- sf::st_read(dsn = "/home/ricardo/Documents/mapas/shapes/BRA_adm/")
plot(bra_adm[5])

# Filtrar estados
estados <- bra_adm %>% 
    dplyr::filter(NAME_1 %in% c("Bahia", "Ceará", "Paraíba", "Pernambuco", 
                                "Piauí", "Rio Grande do Norte", "Sergipe"))
    
# Carregando shape América do Sul
amsul <- sf::st_read(dsn = "/home/ricardo/Documents/mapas/shapes/samerica")
plot(amsul)



# 3) Plot simples ---------------------------------------------------------

ggplot() +
    geom_sf(data = amsul, size = 0.6, color = "black", fill = "white") +
    geom_sf(data = bra_adm, alpha = 0.2, size = 0.2, color = "black",
            fill = "#6A956D") +
    geom_sf(data = estados, alpha = 0.5, fill = "red") +
    coord_sf(xlim = c(-95, -35), ylim = c(10, -50),
             crs = "EPSG:4674") +
    geom_point(data = data, aes(x = long, y = lat)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(panel.background = element_rect("#edf8fb"))



















