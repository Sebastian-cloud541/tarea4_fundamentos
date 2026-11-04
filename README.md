# T4 — Portafolio de visualización con ggplot2

**Fundamentos de Programación para Análisis Económico · UdeC-EAN**
Sebastián Navarrete Poblete 

## Cómo reproducirlo

1. Abrir el proyecto desde el `.Rproj` (las rutas son relativas: `data/raw/` y `output/figuras/`).
2. Paquetes: `dplyr`, `tidyr`, `ggplot2`, `scales`, `ggrepel`, `patchwork`.
3. Correr `scripts/t4_parte1.R` y `scripts/t4_parte2.R` de principio a fin. Las figuras se guardan en `output/figuras/` con `ggsave()` a 300 dpi.

Datos: `casen_reducido.csv`, `salario_minimo_chile.csv` y `ene-2026-06-mjj.csv`.

## Parte 1 (Semana 10)

| Figura | Qué muestra | Hallazgo |
| `t4p1_g1_casen_educ_ingreso.png` | Ingreso según años de educación, con una recta por género (CASEN). | Cada año de educación suma más ingreso a las mujeres (+$46.862) que a los hombres (+$38.295), pero ellas parten más abajo. |
| `t4p1_g2_salario_nominal_real.png` | Ingreso mínimo nominal y real (pesos de 2010), 2014-2025. | El mínimo nominal subió 135 % , pero su poder de compra solo 44 % . |
| `t4p1_g3_ene_categoria_sexo.png` | Composición de los ocupados por categoría ocupacional según sexo (ENE, ponderado). | Las mujeres trabajan más en el sector público y en servicio doméstico . |

## Parte 2 (Semana 11)

| Figura | Qué muestra | Hallazgo |
| `t4p2_g1_casen_scatter.png` | Scatter CASEN de referencia: educación e ingreso por género. | A más educación, más ingreso, y la brecha de género se cierra en los niveles altos. |
| `t4p2_g2_informalidad_region.png` | Tasa de informalidad por región, con Biobío resaltado y el promedio nacional. | Biobío (27,6 %) queda apenas 1 punto sobre el promedio de Chile. La Araucanía, Ñuble y Los Ríos superan el 33 %. |
| `t4p2_g3_horas_sexo.png` | Densidad ponderada de las horas habituales por sexo, sin 888/999. | Ambos tienen mediana de 42 h, pero el 24,2 % de las mujeres trabaja menos de 30 h, contra el 13,3 % de los hombres. |
| `t4p2_g4_hombres_vs_mujeres.png` | Informalidad de hombres contra mujeres por región, con la diagonal y = x. | En 12 de 16 regiones las mujeres son más informales. Las brechas mayores están en Tarapacá y Atacama . |
| `t4p2_g5_informalidad_edad_sexo.png` | **Gráfico libre:** | informalidad por tramo de edad y sexo. |
| `t4p2_figura_compuesta.png` | Figura compuesta (patchwork)| Uno de cada cuatro ocupados en Chile es informal, y la brecha de género se abre en el norte y después de los 60. |

## Reflexión
 1) ¿Cuál comunica MEJOR su hallazgo? El gráfico 2. Tiene
    una sola variable cuantitativa codificada en largo desde 0 (la codificación
    más precisa para comparar), barras ORDENADAS por valor, UN color con función
    (Biobío en dorado, el resto en gris de contexto), los valores escritos y la
    línea del promedio nacional.

 2) ¿Por qué densidad en el gráfico 3? Porque las medianas son IGUALES (42 h):
    un boxplot mostraría dos cajas casi iguales y escondería la diferencia real,
    que está en la FORMA de la distribución (la masa de jornadas parciales de
    las mujeres bajo 30 h). El violín muestra la forma, pero espejada y en dos
    columnas; dos densidades superpuestas en el mismo eje permiten comparar
    directo.

 3) ¿Qué agrega la figura compuesta? Cuenta una historia en tres pasos que
    ningún gráfico solo cuenta: (A) cuánta informalidad hay y dónde, (B) que en
    casi todas las regiones las mujeres están peor y que la brecha es mayor en
    el norte, (C) en qué momento de la vida se abre esa brecha. Los tres paneles
    comparten fuente, paleta y unidad , así que se leen como un
    solo argumento, con un título general que dice el hallazgo de conjunto.