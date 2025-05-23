rm(list = ls())

# Librerías necesarias
#install.packages(c("worldfootballR", "dplyr", "ggplot2", "ggimage", "stringr"))
library(worldfootballR)
library(dplyr)
library(ggplot2)
library(ggimage)
library(stringr)

# install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR", force = TRUE)
# library(worldfootballR)


# Descargar estadísticas de tiros y posesión
shots <- fb_season_team_stats(
  country = "ARG",
  gender = "M",
  season_end_year = 2025,
  tier = "1st",
  stat_type = "shooting"
)

poss <- fb_season_team_stats(
  country = "ARG",
  gender = "M",
  season_end_year = 2025,
  tier = "1st",
  stat_type = "possession"
)

stand <- fb_season_team_stats(
  country = "ARG",
  gender = "M",
  season_end_year = 2025,
  tier = "1st",
  stat_type = "standard"
)


stats <- stand %>% 
  select(Squad, Poss, Gls, xG_Expected) %>% 
  slice(1:30)

stats_aux <- stand %>% 
  select(Squad, xG_Expected) %>%
  rename(xGA = xG_Expected) %>% 
  slice(31:60) %>% 
  mutate(Squad = gsub("^\\s*[Vv][Ss]?\\s*", "", Squad))

stats_def <- stats %>% 
  left_join(stats_aux)

# # Combinar estadísticas
# arg_stats <- shots %>%
#   select(Squad, Shots = `Sh_Standard`) %>%
#   inner_join(
#     poss %>% select(Squad, Poss = `Poss`),
#     by = "Squad"
#   ) %>%
#   mutate(Poss = as.numeric(str_remove(Poss, "%"))) %>% 
#   slice(1:30) %>% #selecciono las primeras 30 
#   mutate(
#       Equipo = str_to_lower(Squad)) %>% 
#   select(!c(Squad)) %>% 
#   relocate(Equipo)



# # Unir datos con escudos
# arg_plot_data <- arg_stats %>%
#   inner_join(logos, by = "Squad")

# Lista archivos con sus rutas
archivos <- list.files("escudos/", pattern = "\\.png$", full.names = TRUE)
nombres <- tools::file_path_sans_ext(basename(archivos))

# Crear un data frame con nombres y rutas
df_imagenes <- data.frame(equipo = nombres, ruta_imagen = archivos, stringsAsFactors = FALSE)

# Unir con tu data frame original
df <- merge(df, df_imagenes, by = "equipo", all.x = TRUE)

# FALTA IGUALAR LOS NOMBRES

df_imagenes <- df_imagenes %>% 
  mutate(nombre_equipo = case_when(equipo == "aldosivi" ~ "Aldosivi",
                                   equipo == "argentinos" ~ "Arg Juniors",
                                   equipo == "atleticotucuman" ~"Atlé Tucumán",
                                   equipo == "banfield" ~ "Banfield",
                                   equipo == "barracas" ~ "Barracas Central",
                                   equipo == "belgrano" ~ "Belgrano",
                                   equipo == "boca" ~ "Boca Juniors",
                                   equipo == "centralcordoba" ~ "Cen. Córdoba–SdE", 
                                   equipo == "defensa" ~ "Defensa y Just",
                                   equipo == "estudiantes" ~ "Estudiantes–LP", 
                                   equipo == "gimnasia" ~ "Gimnasia–LP",
                                   equipo == "godoycruz" ~ "Godoy Cruz", 
                                   equipo == "huracan" ~ "Huracán", 
                                   equipo == "independiente" ~ "Independiente",
                                   equipo == "independienteriv" ~ "Ind. Rivadavia",
                                   equipo == "instituto"~"Instituto",
                                   equipo == "lanus"~ "Lanús",
                                   equipo == "newells" ~"Newell's OB",
                                   equipo == "platense" ~ "Platense",
                                   equipo == "racing" ~ "Racing Club",  
                                   equipo == "riestra" ~ "Deportivo Riestra",
                                   equipo == "river" ~ "River Plate",
                                   equipo == "rosariocentral" ~ "Rosario Central",
                                   equipo == "sanlorenzo" ~ "San Lorenzo",
                                   equipo == "sanmartinsj" ~ "San Martín de San Juan",
                                   equipo == "sarmiento" ~ "Sarmiento",
                                   equipo == "talleres" ~ "Talleres", 
                                   equipo == "tigre" ~ "Tigre",
                                   equipo == "union"~ "Unión",
                                   equipo == "velez" ~"Vélez Sarsfield")) %>% 
  select(nombre_equipo,ruta_imagen) %>% 
  rename(Squad = nombre_equipo)



stats <- stats_def %>% 
  left_join(df_imagenes)

write.csv(stats, "data/stats.csv")

# Crear gráfico
xG_vs_xGA <- ggplot(stats, aes(x = xG_Expected, y = xGA)) +
  geom_image(aes(image = ruta_imagen), size = 0.07) +
  geom_vline(xintercept = mean(stats$xG_Expected), linetype = "dashed", color = "grey") +
  geom_hline(yintercept = mean(stats$xGA), linetype = "dashed", color = "grey") +
  labs(
    title = "Tiros por Partido vs Posesión – Liga Argentina 2023/24",
    x = "Tiros por Partido",
    y = "Posesión (%)"
  ) +
  theme_minimal()

ggsave("xG_vs_xGA.png", xG_vs_xGA)
