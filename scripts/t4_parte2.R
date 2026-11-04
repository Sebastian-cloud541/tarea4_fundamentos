# =============================================================================
# T4 — Portafolio de visualización con ggplot2 · PARTE 2
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: Sebastián Navarrete Poblete
# Fecha: 03-11-2026
# Abrir desde el .Rproj: rutas relativas (data/raw/, output/figuras/).
# =============================================================================



# PAQUETES
# Si es la primera vez:
# install.packages(c("dplyr", "ggplot2", "scales", "ggrepel", "patchwork"))

library(dplyr)
library(ggplot2)
library(scales)      
library(ggrepel)     
library(patchwork)   # componer figuras 

azul <- "#223c6a"; oro <- "#e69b0a"; gris <- "grey75"

pesos <- label_currency(big.mark = ".", decimal.mark = ",")
pct   <- label_number(accuracy = 0.1, decimal.mark = ",", suffix = " %")  # valores ya en %

# Tema propio como funcion y activado para todos los gráficos.
tema_fppe <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", color = azul),
          plot.subtitle    = element_text(color = "grey40"),
          plot.caption     = element_text(color = "grey50", hjust = 0),
          plot.background  = element_rect(fill = "white", color = NA),  # PNG con fondo blanco
          legend.position  = "bottom")
}
theme_set(tema_fppe())

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

fuente_ene <- "Fuente: INE, Encuesta Nacional de Empleo"



# DATOS

casen <- read.csv("data/raw/casen_reducido.csv") |> filter(!is.na(ingreso))

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

# Ocupados de la ENE (Semana 6): activ == 1, variables lógicas y región en texto.
# Las horas se limpian de los centinelas 888 y 999 (no sabe / no responde).
ocupados <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1) |>
  mutate(informal   = ocup_form == 2,
         mujer      = sexo == 2,
         sexo_txt   = ifelse(mujer, "Mujeres", "Hombres"),
         region_txt = regiones[region],
         horas      = na_if(na_if(habituales, 888), 999))

nrow(casen); nrow(ocupados); sum(ocupados$fact_cal)
# 55 personas en CASEN (sin los 5 NA)

# Informalidad nacional, ponderada (se usa como línea de referencia)
inf_nacional <- 100 * weighted.mean(ocupados$informal, ocupados$fact_cal)
inf_nacional
# 26,5 %: uno de cada cuatro ocupados es informal.


# GRÁFICO 1 · Scatter CASEN 

# color = genero en el aes() GLOBAL -> lo hereda geom_smooth(): una recta por género.
coef(lm(ingreso ~ educ, data = filter(casen, genero == "F")))    # 46862 por año
coef(lm(ingreso ~ educ, data = filter(casen, genero == "M")))    # 38295 por año

g1 <- ggplot(casen, aes(educ, ingreso, color = genero)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  annotate("text", x = 20.1, y = 1030000, label = "Mujeres", color = oro,
           hjust = 0, size = 4.5) +
  annotate("text", x = 19.2, y = 850000, label = "Hombres", color = azul,
           hjust = 0, size = 4.5) +
  scale_color_manual(values = c(F = oro, M = azul), guide = "none") +
  scale_x_continuous(breaks = seq(6, 20, 2), limits = c(6, 22.5)) +
  scale_y_continuous(labels = pesos, limits = c(0, NA)) +
  labs(title    = "A más educación, más ingreso" ,
       subtitle = "Pendiente: 46862 por año de educación (mujeres) y 38295 (hombres)",
       x = "Años de educación", y = "Ingreso mensual",
       caption  = "Fuente: CASEN")
g1

ggsave("output/figuras/t4p2_g1_casen_scatter.png", g1,
       width = 9, height = 5.5, dpi = 300)


# GRÁFICO 2 · Informalidad por región, con UNA región resaltada (Biobío)

# Informalidad PONDERADA por región, total y por sexo.
inf_region <- ocupados |>
  group_by(region_txt) |>
  summarise(total   = 100 * weighted.mean(informal, fact_cal),
            hombres = 100 * weighted.mean(informal[!mujer], fact_cal[!mujer]),
            mujeres = 100 * weighted.mean(informal[mujer],  fact_cal[mujer])) |>
  mutate(brecha = mujeres - hombres)

inf_region |> arrange(desc(total))
# La Araucanía 35,0 · Ñuble 34,5 · Los Ríos 33,2 ... Biobío 27,6 (lugar 11 de
# 16) ... Metropolitana 23,6 · Antofagasta 21,1 · Magallanes 21,1.
# Biobío: 27,6 %, 1,1 puntos sobre el promedio nacional (26,5 %).

g2 <- inf_region |>
  mutate(destacar = region_txt == "Biobío") |>              
  ggplot(aes(total, reorder(region_txt, total), fill = destacar)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = pct(total)), hjust = -0.15, size = 3.4) +   # valores escritos
  geom_vline(xintercept = inf_nacional, linetype = "dashed", color = "grey30") +
  annotate("text", x = inf_nacional + 0.4, y = 1.5, hjust = 0, size = 3.4,
           color = "grey30", label = "Promedio de Chile: 26,5 %") +
  scale_fill_manual(values = c(`TRUE` = oro, `FALSE` = gris), guide = "none") +
  scale_x_continuous(limits = c(0, 40),                       # barras DESDE 0
                     labels = label_number(suffix = " %")) +
  labs(title    = "Biobío en el medio: 27,6 % de informalidad",
       subtitle = "Araucanía, Ñuble y Los Ríos superan el 33 %",
       x = "% de ocupados informales", y = NULL,
       caption  = fuente_ene) +
  theme(panel.grid.major.y = element_blank())
g2

ggsave("output/figuras/t4p2_g2_informalidad_region.png", g2,
       width = 9, height = 6.5, dpi = 300)



# GRÁFICO 3 · Distribución de las horas habituales por sexo

# Primero, los centinelas: un histograma de "habituales" tal como viene estira
# el eje hasta 999 (S11S1, Bloque A).
max(ocupados$habituales); sum(ocupados$habituales %in% c(888, 999))
max(ocupados$horas, na.rm = TRUE)
# Máximo 999 con 111 centinelas. Limpias, el máximo es 126.

horas <- ocupados |> filter(!is.na(horas))

# Los números detrás del gráfico (ponderados)
horas |>
  group_by(sexo_txt) |>
  summarise(media    = round(weighted.mean(horas, fact_cal), 1),
            mediana  = median(horas),
            menos_30 = round(100 * weighted.mean(horas < 30, fact_cal), 1),
            de_40_45 = round(100 * weighted.mean(horas >= 40 & horas <= 45, fact_cal), 1),
            mas_90   = round(100 * weighted.mean(horas > 90, fact_cal), 2))
#    Hombres: media 40,5 h, mediana 42, 13,3 % trabaja menos de 30 h.
#    Mujeres: media 36,4 h, mediana 42, 24,2 % trabaja menos de 30 h.
#    Más de 90 h: 0,15 % de los hombres y 0,06 % de las mujeres.

# ELECCIÓN DEL GEOM: densidad ponderada, las dos superpuestas.
#   - Las MEDIANAS son iguales (42 h): un boxplot mostraría dos cajas casi
#     idénticas y escondería lo importante, las de jornadas parciales
#     de las mujeres a la izquierda de 30 h.
#   - Un violín también la mostraría, pero espejado y en dos columnas separadas;
#     superponer las dos densidades en el MISMO eje X compara directo.

g3 <- ggplot(horas, aes(horas, fill = sexo_txt, color = sexo_txt, weight = fact_cal)) +
  geom_density(alpha = 0.35, bw = 2, linewidth = 0.8) +
  geom_vline(xintercept = 42, linetype = "dashed", color = "grey30") +
  annotate("text", x = 43, y = 0.105, hjust = 0, size = 3.4, color = "grey30",
           label = "Jornada legal: 42 h") +
  # etiquetas directas en el color de cada curva
  annotate("text", x = 20, y = 0.052, size = 3.8, color = azul,
           label = "Hombres") +
  annotate("text", x = 20, y = 0.03, size = 3.8, color = oro,
           label = "Mujeres") +
  scale_fill_manual(values = c(Mujeres = oro, Hombres = azul)) +
  scale_color_manual(values = c(Mujeres = oro, Hombres = azul)) +
  scale_x_continuous(breaks = seq(0, 90, 10), labels = label_number(suffix = " h")) +
  scale_y_continuous(labels = label_number(accuracy = 0.001, decimal.mark = ",")) +
  coord_cartesian(xlim = c(0, 90)) +   
  labs(title    = "1 de cada 4 mujeres trabaja menos de 30 horas; 1 de cada 8 hombres",
       subtitle = "Distribución de las horas habituales por semana.",
       x = "Horas habituales por semana", y = "Densidad",
       caption  = "Fuente: INE, Encuesta Nacional de Empleo") +
  theme(legend.position = "none")
g3

ggsave("output/figuras/t4p2_g3_horas_sexo.png", g3,
       width = 9.5, height = 5.5, dpi = 300)



# GRÁFICO 4 · Informalidad de hombres contra mujeres por región (ggrepel)

sum(inf_region$brecha > 0)
inf_region |> arrange(desc(brecha)) |> select(region_txt, hombres, mujeres, brecha)
#    En 12 de 16 regiones la informalidad de las mujeres supera a la de los
#    hombres.

# El color tiene función: dorado para las regiones con brecha > 10 pp, el resto azul.
g4 <- inf_region |>
  mutate(brecha_grande = brecha > 10) |>
  ggplot(aes(hombres, mujeres)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +  # y = x
  annotate("text", x = 18.5, y = 37, hjust = 0, size = 3.4, color = "grey40",
           label = "Mujeres más informales") +
  annotate("text", x = 37, y = 23, hjust = 1, size = 3.4, color = "grey40",
           label = "Hombres más informales") +
  geom_point(aes(color = brecha_grande), size = 2.8) +
  geom_text_repel(aes(label = region_txt, color = brecha_grande), size = 3.2,
                  max.overlaps = Inf, seed = 1) +                  
  scale_color_manual(values = c(`TRUE` = oro, `FALSE` = azul), guide = "none") +
  scale_x_continuous(labels = label_number(suffix = " %"), breaks = seq(15, 40, 5)) +
  scale_y_continuous(labels = label_number(suffix = " %"), breaks = seq(15, 40, 5)) +
  coord_cartesian(xlim = c(17, 38), ylim = c(17, 38)) +   
  labs(title    = "En 12 de 16 regiones las mujeres son más informales",
       subtitle = "Las brechas mayores están en: Tarapacá y Atacama",
       x = "Informalidad de los hombres", y = "Informalidad de las mujeres",
       caption  = fuente_ene)
g4

ggsave("output/figuras/t4p2_g4_hombres_vs_mujeres.png", g4,
       width = 8, height = 8, dpi = 300)



# GRÁFICO 5 · LIBRE: ¿a qué edad es más alta la informalidad, y dónde se abre
#             la brecha entre mujeres y hombres?

# Mi pregunta:la "U" de la informalidad por edad para todos.
# ¿Es la misma U para mujeres y hombres? 
#
# Tramos de 5 años 
edad_sexo <- ocupados |>
  mutate(tramo = pmin(floor(edad / 5) * 5, 70)) |>
  group_by(sexo_txt, tramo) |>
  summarise(tasa = 100 * weighted.mean(informal, fact_cal),
            n    = n(), .groups = "drop") ; edad_sexo

min(edad_sexo$n)
edad_sexo |> filter(tramo >= 55)
#    todos los tramos tienen más de 150 casos.

g5 <- ggplot(edad_sexo, aes(tramo, tasa, color = sexo_txt, linetype = sexo_txt)) +
  geom_hline(yintercept = inf_nacional, color = "grey60") +
  annotate("text", x = 71, y = inf_nacional - 4, hjust = 0, size = 3.2,
           color = "grey40", label = "Promedio Chile") +
  geom_vline(xintercept = 60, linetype = "dotted", color = "grey40") +
  annotate("text", x = 60.5, y = 8, hjust = 0, size = 3.2, color = "grey30",
           label = "60-64 años") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  annotate("text", x = 71, y = 65.6, hjust = 0, size = 3.8, color = oro, label = "Mujeres") +
  annotate("text", x = 71, y = 60.0, hjust = 0, size = 3.8, color = azul, label = "Hombres") +
  scale_color_manual(values = c(Mujeres = oro, Hombres = azul)) +
  scale_x_continuous(breaks = seq(15, 70, 5),
                     labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                                "45-49", "50-54", "55-59", "60-64", "65-69", "70+"),
                     limits = c(15, 76)) +
  scale_y_continuous(labels = label_number(suffix = " %"), limits = c(0, 72)) +
  labs(title    = "Desde los 60 años, la mitad o más de las mujeres ocupadas es informal",
       subtitle = "Informalidad por tramo de edad y sexo",
       x = "Tramo de edad", y = "% de ocupados informales",
       caption  = fuente_ene) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9))
g5

ggsave("output/figuras/t4p2_g5_informalidad_edad_sexo.png", g5,
       width = 10, height = 5.5, dpi = 300)



# FIGURA COMPUESTA · gráficos 2, 4 y 5 con patchwork
# individuales: la fuente va UNA vez, en la caption general.
p_a <- g2 + labs(title = "Biobío, cerca del promedio; el sur, sobre 33 %",
                 subtitle = NULL, caption = NULL)
p_b <- g4 + labs(title = "En 12 de 16 regiones ellas son más informales",
                 subtitle = NULL, caption = NULL)
p_c <- g5 + labs(title = "Después de los 60 la brecha se dispara",
                 subtitle = NULL, caption = NULL, x = NULL)

# Distribución: arriba las dos vistas por REGIÓN lado a lado (A y B); abajo, a
# todo el ancho, la vista por EDAD (C), que necesita espacio para los 12 tramos.
# (Probé primero p_a | (p_b / p_c), como en clase, pero el scatter quedaba muy
# chico y las etiquetas de ggrepel se encimaban.)
figura <- ((p_a | p_b) / p_c) +
  plot_layout(heights = c(1.35, 1)) +
  plot_annotation(
    title    = "Uno de cada cuatro ocupados en Chile es informal, y la brecha de género se abre en el norte y después de los 60",
    subtitle = "Encuesta Nacional de Empleo, mayo-julio 2026 · ponderado con fact_cal",
    caption  = "Fuente: INE, ENE mayo-julio 2026. Elaboración propia. Ocupados (activ = 1), cifras ponderadas con fact_cal.",
    tag_levels = "A")
figura

ggsave("output/figuras/t4p2_figura_compuesta.png", figura,
       width = 15, height = 13, dpi = 300)



# -----------------------------------------------------------------------------
# REFLEXIÓN (también en el README.md)
# -----------------------------------------------------------------------------
# 1) ¿Cuál comunica MEJOR su hallazgo? El gráfico 2. Tiene
#    una sola variable cuantitativa codificada en largo desde 0 (la codificación
#    más precisa para comparar), barras ORDENADAS por valor, UN color con función
#    (Biobío en dorado, el resto en gris de contexto), los valores escritos y la
#    línea del promedio nacional.
#
# 2) ¿Por qué densidad en el gráfico 3? Porque las medianas son IGUALES (42 h):
#    un boxplot mostraría dos cajas casi iguales y escondería la diferencia real,
#    que está en la FORMA de la distribución (la masa de jornadas parciales de
#    las mujeres bajo 30 h). El violín muestra la forma, pero espejada y en dos
#    columnas; dos densidades superpuestas en el mismo eje permiten comparar
#    directo.
#
# 3) ¿Qué agrega la figura compuesta? Cuenta una historia en tres pasos que
#    ningún gráfico solo cuenta: (A) cuánta informalidad hay y dónde, (B) que en
#    casi todas las regiones las mujeres están peor y que la brecha es mayor en
#    el norte, (C) en qué momento de la vida se abre esa brecha. Los tres paneles
#    comparten fuente, paleta y unidad , así que se leen como un
#    solo argumento, con un título general que dice el hallazgo de conjunto.

