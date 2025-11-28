# Pacotes ----

library(sf)

library(tidyverse)

library(magrittr)

library(elevatr)

library(tidyterra)

# Dados ----

## APA Aldeia Beberibe ----

### Importando ----

apa <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/Ecologia de Paisagens/apa_aldeiabeberibe.shp")

### Visualizando ----

apa

apa |>
  ggplot() +
  geom_sf()

### Tratanndo ----

apa %<>%
  sf::st_transform(crs = 32725)

apa

apa |>
  ggplot() +
  geom_sf()

## Pontos aleatórios -----

### Criando ----

set.seed(123); apa |>
  sf::st_sample(size = 20) -> pontos

### Visualizando ----

pontos

ggplot() +
  geom_sf(data = apa) +
  geom_sf(data = pontos)

## Topografia da APA Adeldeia Beberibe ----

### Importando ----

topo <- elevatr::get_elev_raster(locations = apa,
                                 z = 14,
                                 clip = "locations",
                                 prj = 32725) |>
  terra::rast()

### Visualizando ----

topo

ggplot() +
  tidyterra::geom_spatraster(data = topo) +
  scale_fill_viridis_c(na.value = "transparent",
                       name = "Altitude") +
  geom_sf(data = apa, fill = NA) +
  geom_sf(data = pontos)

### Exportando ----

topo |>
  terra::writeRaster("topo_aldeia_beribe.tif")

# Distâncias ----

## Distância geodésica simples ----

dist_simples <- pontos |> sf::st_distance(apa |>
                                            sf::st_boundary()) |>
  as.data.frame() |>
  dplyr::rename("Distância" = 1) |>
  dplyr::mutate(Distância = Distância |> as.numeric(),
                `Tipo de distância` = "Simples")

dist_simples

## Distância geodésica considerando a topografia ----

### Calculando o ponto da borda mais próximo de todos os pontos ----

near_dist <- pontos |> sf::st_nearest_points(apa |>
                                               sf::st_boundary()) |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::slice(2,
               .by = L1) |>
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 32725)

near_dist

ggplot() +
  geom_sf(data = apa) +
  geom_sf(data = pontos) +
  geom_sf(data = near_dist, color = "red")

### Extraindo os valores de altitude ----

#### Pontos aleatórios ----

alt_alea <- topo |>
  terra::extract(pontos |> sf::st_as_sf()) |>
  dplyr::rename("Altitude" = 2)

alt_alea

#### Pontos mais p´rooximos da borda ----

alt_prox <- topo |>
  terra::extract(near_dist |> sf::st_as_sf()) |>
  dplyr::rename("Altitude" = 2)

alt_prox

### Delta de altitude ----

delta_alt <- alt_alea - alt_prox

delta_alt

### Calculando a distância ----

dist_top <- sqrt(dist_simples$Distância^2 + delta_alt$Altitude^2) |>
  as.data.frame() |>
  dplyr::rename("Distância" = 1) |>
  dplyr::mutate(Distância = Distância |> as.numeric(),
                `Tipo de distância` = "Topográfica")

dist_top

# Comparação -----

## Unindo os dados

dist <- dplyr::bind_rows(dist_simples,
                         dist_top)

dist

## Gráfico ----

dist |>
  ggplot(aes(`Tipo de distância`, Distância, fill = `Tipo de distância`)) +
  ggbeeswarm::geom_quasirandom(shape = 21, stroke = 1,
                               size = 5, color = "black") +
  scale_fill_manual(values = c("orange", "royalblue")) +
  theme_classic()

