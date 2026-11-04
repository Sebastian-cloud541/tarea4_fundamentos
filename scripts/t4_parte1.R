# =============================================================================
# T4 — Portafolio de visualización con ggplot2 · PARTE 1
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: Sebastián Navarrete Poblete
# Fecha: 03-11-2026
#
# Tres gráficos:
#   1. CASEN  — educación e ingreso por género (herencia de aes())
#   2. Ingreso mínimo — nominal contra real 
#   3. ENE    — en qué trabajan hombres y mujeres 
#
#  Abrir desde el .Rproj: todas las rutas son relativas a la carpeta del
#  proyecto (data/raw/ y output/figuras/). Sin setwd() ni rutas absolutas.
# =============================================================================



#  PAQUETES


library(dplyr)
library(tidyr)      # pivot_longer() (Semana 6)
library(ggplot2)
library(scales)     # label_currency(), label_number(), label_percent() (S10S2)

# Colores del curso: azul y dorado se distinguen bien con daltonismo
azul <- "#223c6a"
oro  <- "#e69b0a"

# Formato:
pesos <- label_currency(big.mark = ".", decimal.mark = ",")

# Tema reutilizable guardado en un objeto:
tema_curso <- theme_minimal(base_size = 13) +
  theme(plot.title       = element_text(face = "bold", color = azul),
        plot.subtitle    = element_text(color = "grey40"),
        plot.caption     = element_text(color = "grey50", hjust = 0),
        plot.background  = element_rect(fill = "white", color = NA),  # PNG con fondo blanco
        panel.grid.minor = element_blank(),
        legend.position  = "bottom")

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)



# DATOS

casen   <- read.csv("data/raw/casen_reducido.csv")
salario <- read.csv("data/raw/salario_minimo_chile.csv")
ene     <- read.csv2("data/raw/ene-2026-06-mjj.csv")        

nrow(casen); sum(is.na(casen$ingreso))
# 60 personas y 5 NA en ingreso. 
casen <- casen |> filter(!is.na(ingreso))



# GRÁFICO 1 · CASEN: educación e ingreso por género

# La HERENCIA de aes():
#   - color = genero en ggplot(aes(...))  -> lo heredan TODAS las capas:
#     geom_point() pinta por género y geom_smooth() ajusta UNA RECTA POR GÉNERO.
#   - si color = genero fuera solo en geom_point(aes(...)), geom_smooth() no
#     sabría de los grupos y dibujaría UNA sola recta para todos.

# Las rectas por género
coef(lm(ingreso ~ educ, data = casen))                           
coef(lm(ingreso ~ educ, data = filter(casen, genero == "F")))    # mujeres
coef(lm(ingreso ~ educ, data = filter(casen, genero == "M")))    # hombres
# todos: 42079 por año, Mujeres: 46862 por año (intercepto 32295),
# Hombres: 38294 por año

g1 <- ggplot(casen, aes(educ, ingreso, color = genero)) +   # color GLOBAL -> se hereda
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +  # una recta por género
  annotate("text", x = 20.1, y = 1030000, label = "Mujeres", color = oro,
           hjust = 0, size = 4.5) +
  annotate("text", x = 19.2, y = 850000, label = "Hombres", color = azul,
           hjust = 0, size = 4.5) +
  scale_color_manual(values = c(F = oro, M = azul), guide = "none") +
  scale_x_continuous(breaks = seq(6, 20, 2), limits = c(6, 22.5)) +
  scale_y_continuous(labels = pesos, limits = c(0, NA)) +
  labs(title    = "Cada año de educación suma más ingreso a las mujeres que a los hombres",
       subtitle = "Pendiente: por año (mujeres) y (hombres)" ,
       x        = "Años de educación",
       y        = "Ingreso mensual",
       caption  = "Fuente: CASEN") +
  tema_curso
g1

ggsave("output/figuras/t4p1_g1_casen_educ_ingreso.png", g1,
       width = 9, height = 5.5, dpi = 300)

# PREGUNTA: ¿qué pregunta responde este gráfico que no respondería la
#    versión con una sola recta?
# RESPUESTA: con una sola recta (pendiente 42080 para todos) solo se responde
#   "¿cuánto aumenta el ingreso con un año más de educación?". Con una recta por
#   género se responde "¿el retorno de la educación es igual para mujeres y
#   hombres?" y "¿dónde está la brecha?". Aquí la recta de las mujeres es más
#   empinada (46.862 contra 38.295 por año) pero parte más abajo (intercepto
#   32.295 contra 193.555): con poca educación la brecha favorece a los
#   hombres y se cierra a medida que sube la escolaridad.


# GRÁFICO 2 · Ingreso mínimo: nominal contra real

# Real = nominal / IPC * 100 
# Dos series en un gráfico -> formato LARGO (Semana 6): una fila por año y serie.
salario_largo <- salario |>
  mutate(Real = salario_minimo / ipc * 100) |>
  select(anio, Nominal = salario_minimo, Real) |>
  pivot_longer(-anio, names_to = "serie", values_to = "pesos")

salario_largo
# 24 filas: 12 años x 2 series.

# Los extremos de cada serie (para el título y las etiquetas)
salario_largo |> filter(anio %in% c(2014, 2025))
# Nominal: 225000 -> 529000 · Real: 198238 -> 285175
round(100 * (529000 / 225000 - 1), 1)        # 135,1 
round(100 * (285175 / 198238 - 1), 1)        # 43,9 

g2 <- ggplot(salario_largo, aes(anio, pesos, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.2) +       
  geom_point(size = 1.8) +
  # etiquetas directas al final de cada línea (reemplazan la leyenda)
  annotate("text", x = 2025.25, y = 529000, hjust = 0, size = 4, color = azul,
           label = "Nominal") +
  annotate("text", x = 2025.25, y = 285175, hjust = 0, size = 4, color = oro,
           label = "Real") +
  scale_color_manual(values = c(Nominal = azul, Real = oro)) +
  scale_x_continuous(breaks = seq(2014, 2025, 1), limits = c(2014, 2026.6)) +
  scale_y_continuous(labels = pesos) +
  labs(title    = "El mínimo subió 135 %, pero su poder de compra solo 44 %",
       subtitle = "Ingreso mínimo mensual, 2014-2025 · real en pesos de 2010 (deflactado por IPC)",
       x        = NULL,
       y        = "Pesos por mes",
       caption  = "Fuente:  (INE) salario_minimo_chile.csv ") +
  tema_curso +
  theme(legend.position = "none")    
g2

ggsave("output/figuras/t4p1_g2_salario_nominal_real.png", g2,
       width = 9, height = 5.5, dpi = 300)

# PREGUNTA: ¿por qué aquí el eje Y no necesita partir en 0 y en un gráfico
#    de barras sí?
# RESPUESTA: en una barra el dato se lee por su LARGO : si el
#   eje parte en 200.000, una barra del doble de valor se vería varias veces más
#   larga y el gráfico mentiría. En una línea el dato se
#   lee por la POSICIÓN y la PENDIENTE: lo que importa es la evolución y la
#   distancia entre las dos series, no el largo desde cero. Por eso basta con
#   que el eje esté rotulado y en pesos ("Pesos por mes", $200.000, $300.000...)
#   y que se muestre el RANGO COMPLETO de años (2014-2025).
#
# PREGUNTA: ¿qué ganas usando color Y tipo de línea?
# RESPUESTA: codificación REDUNDANTE. Si el gráfico se imprime en blanco y negro,
#   o lo mira una persona con daltonismo, la serie real se sigue distinguiendo
#   por ser punteada. Además, como las etiquetas directas van al final de cada
#   línea, no hace falta leyenda.


# GRÁFICO 3 · ENE: en qué trabajan hombres y mujeres

# Ocupados (activ == 1), sexo y categoría ocupacional legibles.
ocupados <- ene |>
  filter(activ == 1) |>
  mutate(sexo = ifelse(sexo == 1, "Hombres", "Mujeres"),
         categoria = case_when(categoria_ocupacion == 3      ~ "Asalariado privado",
                               categoria_ocupacion == 4      ~ "Asalariado público",
                               categoria_ocupacion == 2      ~ "Cuenta propia",
                               categoria_ocupacion == 1      ~ "Empleador",
                               categoria_ocupacion %in% 5:6  ~ "Servicio doméstico",
                               TRUE                          ~ "Familiar no remunerado"))

nrow(ocupados); sum(ocupados$fact_cal)
# 41850 encuestados que representan 9.314.017 ocupados.

# Las proporciones que dibuja position = "fill", PONDERADAS
ocupados |>
  group_by(sexo, categoria) |>
  summarise(pob = sum(fact_cal), .groups = "drop") |>
  group_by(sexo) |>
  mutate(pct = round(100 * pob / sum(pob), 1)) |>
  arrange(categoria)

# Paleta Okabe-Ito , sin el negro
g3 <- ggplot(ocupados, aes(sexo, fill = categoria, weight = fact_cal)) +  
  geom_bar(position = "fill", width = 0.65) +                             # cada barra = 100 %
  scale_fill_manual(values = unname(palette.colors(7, "Okabe-Ito"))[-1]) +
  scale_y_continuous(labels = label_percent()) +                          # de 0 % a 100 %
  coord_flip() +
  labs(title    = "Las mujeres trabajan más en el sector público y en servicio doméstico",
       subtitle = "Asalariado público: 17,0 % de las mujeres y 9,2 % de los hombres." ,
       x        = NULL,
       y        = "% de los ocupados de cada sexo",
       fill     = NULL,
       caption  = "Fuente: INE, Encuesta Nacional de Empleo") +
  tema_curso +
  theme(panel.grid.major.y = element_blank())
g3

ggsave("output/figuras/t4p1_g3_ene_categoria_sexo.png", g3,
       width = 11, height = 5, dpi = 300)

# PREGUNTA: ¿en qué categoría está la mayor diferencia entre hombres y
#    mujeres, y en cuál casi no hay? Da las cifras.
# RESPUESTA: la mayor diferencia en puntos porcentuales está en ASALARIADO
#   PRIVADO: 64,9 % de los hombres ocupados contra 53,9 % de las mujeres.
#   Le sigue asalariado PÚBLICO, al revés: 17,0 % de las mujeres
#   contra 9,2 % de los hombres. En términos relativos
#   lo más extremo es el SERVICIO DOMÉSTICO: 5,1 % de las mujeres y 0,1 % de los
#   hombres. Donde casi no hay diferencia es en CUENTA PROPIA: 21,1 % de los
#   hombres y 21,2 % de las mujeres.

# PREGUNTA: ¿qué cambiaría si borras weight = fact_cal?
# RESPUESTA: geom_bar() CONTARÍA personas encuestadas en vez
#   de SUMAR los factores de expansión. Las
#   proporciones serían las de la muestra, no las de Chile, y la muestra no es
#   proporcional a la población (hay regiones y grupos sobrerrepresentados).

# Comprobación de la respuesta anterior: la misma tabla SIN ponderar (cuenta filas)
ocupados |>
  count(sexo, categoria) |>
  group_by(sexo) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(categoria)


# -----------------------------------------------------------------------------
# CHECKLIST (S9) aplicado a los tres gráficos
# -----------------------------------------------------------------------------
# [x] Títulos con el hallazgo y cifras, no con los nombres de las variables.
# [x] Barras desde 0 (g3 va de 0 % a 100 %); líneas con eje rotulado en pesos.
# [x] Rango completo: 2014-2025 entero, las 55 personas con ingreso, todos los ocupados.
# [x] Paleta accesible: azul/dorado y Okabe-Ito; nada de rojo contra verde.
# [x] Etiquetas directas en g1 y g2 (sin leyenda).
# [x] Sin chart junk: theme_minimal, sin grilla menor, sin 3D ni fondos.
# [x] Ejes con unidades: label_currency() en pesos y label_percent() en %.
# [x] Fuente en la caption, indicando si está ponderado o no.
# [x] Exportados con ggsave() a 300 dpi.
# =============================================================================
