# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

# Instalamos/checamos paquetes necesarios:
paquetes <- c("ggplot2", "dplyr", "tidyr", "car")
for(p in paquetes) instalar_si_falta(p)

# Cargamos librerías a la sesión actual:
library(ggplot2) # para gráficas modernas (geom_boxplot, geom_violin, facet)
library(dplyr) # para manipulación de datos (filter, group_by, summarise)
library(tidyr) # para pasar datos a formato largo (pivot_longer) si hace falta
library(car) # para pruebas como leveneTest (homogeneidad de varianzas)

getwd()
# Cargar el CSV:
# - stringsAsFactors = FALSE para evitar factores automáticos (en versiones R < 4.0).
# - na.strings especifica cómo interpretar valores perdidos.
datos <- read.csv("datos_biomed.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Comprobamos nombres de columnas disponibles
names(datos)

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

# Primeras filas para ver estructura general
a <- head(datos) # muestra 6 primeras filas (por defecto)
print(a)
# CONCLUSIONES SACADAS:
#El dataset contiene valores biomédicos de varios individuos.
# Hay una columna ID que identifica cada muestra.
# La variable "Tratamiento" indica el grupo experimental.
# Las variables numéricas son Glucosa, Presion y Colesterol.
# Los valores de Glucosa parecen variar entre tratamientos.
# FarmacoB muestra niveles de glucosa algo más altos que Placebo.
# Los valores de Presion son variables entre sujetos.
# Colesterol también muestra diferencias entre tratamientos.


# Resumen estadístico básico de cada variable
b <- summary(datos)
print(b)
#CONCLUSIONES
# Hay 100 observaciones en total y 5 variables (ID, Tratamiento, Glucosa, Presion, Colesterol).
# La variable "Tratamiento" es de tipo carácter (categoría).
# Las variables Glucosa, Presion y Colesterol son numéricas.
# La glucosa varía entre 69.7 y 150.6 mg/dL, con una media de 106.5.
# La presión varía entre 103.3 y 153.9 mmHg, con una media de 126.7.
# El colesterol varía entre 149.3 y 252.0 mg/dL, con una media de 199.9.
# Las medias de las tres variables están dentro de rangos fisiológicos normales.

# Dimensiones: número de filas (observaciones) y columnas (variables)
# dim() devuelve c(n_filas, n_columnas)
dimensiones <- dim(datos)
cat("\nDimensiones del dataset (filas, columnas): ", dimensiones, "\n")
#Tiene 100 filas y 5 columnas

# Estructura interna del objeto (tipos de datos, etc.)
str(datos)

# Convertimos/aseguramos que Tratamiento sea factor para análisis por grupos
if("Tratamiento" %in% names(datos)){
  datos$Tratamiento <- as.factor(datos$Tratamiento)
}

# ¿Cuántas variables hay? → número de columnas
num_variables <- ncol(datos)
cat("\nNúmero de variables (columnas): ", num_variables, "\n")

# ¿Cuántos tratamientos distintos?
num_tratamientos <- nlevels(datos$Tratamiento)
cat("Número de tratamientos distintos: ", num_tratamientos, "\n")
cat("Tratamientos: ", paste(levels(datos$Tratamiento), collapse = ", "), "\n\n")

#hay 3 tratameintos distintos FarmacoA,B y placebo

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
# "Todos los boxplots por tratamiento" significa mostrar la distribución
# de variables (Glucosa, Presion, Colesterol) separadas por Tratamiento.
# Nota: usamos theme_bw() para un estilo neutro.
# VERIFICACIÓN: Comprobamos que existan las columnas numéricas esperadas.
vars_numericas <- c("Glucosa", "Presion", "Colesterol")
faltan <- setdiff(vars_numericas, names(datos))
print(faltan)

# a) Con ggplot2, una por figura 
# Nota: usamos theme_bw() para un estilo neutro.
# Bucle for: recorre cada variable numérica (Glucosa, Presion, Colesterol)
# Recorremos todas las variables numéricas almacenadas en 'vars_numericas'
for(v in vars_numericas){
  # ggplot() inicia el objeto de la gráfica indicando:
  # - datos: el dataframe 'datos'
  # - aes(): las "aesthetics" o correspondencias de variables
  #   x = Tratamiento (en el eje X)
  #   y = .data[[v]] -> accede a la variable cuyo nombre está en 'v'
  #   fill = Tratamiento -> color de relleno según el grupo (tratamiento)
  p <- ggplot(datos, aes(x = Tratamiento, y = .data[[v]], fill = Tratamiento)) +
    # geom_boxplot() crea el diagrama de cajas
    # outlier.alpha = 0.7 ajusta la transparencia de los puntos atípicos
    geom_boxplot(outlier.alpha = 0.7) +
    # labs() añade títulos y etiquetas:
    # - title: usa paste() para generar un título dinámico con el nombre de la variable
    # - x: etiqueta del eje X
    # - y: etiqueta del eje Y (aquí usamos directamente el nombre de la variable)
    labs(title = paste("Boxplot de", v, "por tratamiento"), x = "Tratamiento", y = v) +
    # theme_bw() aplica un tema de fondo blanco (clean black & white) 
    # theme() personaliza elementos estéticos:
    # legend.position = "none" elimina la leyenda (ya se ve en el eje X)
    theme_bw() + theme(legend.position = "none")
  print(p)
}
# b) Alternativa: transformar a formato largo y graficar todos juntos con facet
# Esto te permite ver en un panel los 3 boxplots (uno por variable) por tratamiento.
# 'pivot_longer()' transforma el dataframe de formato ancho (wide)
# a formato largo (long). En formato largo, las columnas se "apilan" verticalmente.
# Esto es útil para ggplot, que puede graficar todas las variables en una sola figura.
datos_largo <- datos |>
  pivot_longer(
    cols = all_of(vars_numericas),   # columnas a transformar (las variables numéricas)
    names_to = "Variable",           # nuevo nombre para la columna que guarda el nombre de la variable original
    values_to = "Valor"              # nuevo nombre para la columna que guarda los valores numéricos
  )

ggplot(datos_largo, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  # geom_boxplot(): crea el diagrama de cajas
  # outlier.alpha = 0.7 hace los puntos atípicos (outliers) más transparentes
  geom_boxplot(outlier.alpha = 0.7) +
  # facet_wrap(~ Variable): crea una "rejilla" de gráficos, 
  # un panel (subplot) distinto para cada variable (Glucosa, Presion, Colesterol)
  # scales = "free_y" permite que cada panel tenga su propia escala de valores
  facet_wrap(~ Variable, scales = "free_y") +
  # labs(): añade título y etiquetas de ejes
  labs(title = "Boxplots por tratamiento - Todas las variables", x = "Tratamiento", y = "Valor") +
  # theme_bw(): aplica un tema de fondo blanco y líneas negras (limpio y clásico)
  # theme(legend.position = "none"): elimina la leyenda (redundante, ya se muestra en el eje X)
  theme_bw() + theme(legend.position = "none")



# 4. Realiza un violin plot (investiga qué es). (1 pt)

# Creamos el gráfico con ggplot
ggplot(datos_largo, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  
  # geom_violin(): genera el gráfico de violín (densidad de los datos por grupo)
  # trim = FALSE -> muestra toda la densidad sin recortar los extremos
  geom_violin(trim = FALSE, alpha = 0.8) +
  
  # geom_boxplot(): añadimos una caja dentro del violín para ver la mediana y cuartiles
  # width = 0.15 -> hace el boxplot más estrecho para que se vea dentro del violín
  # outlier.shape = NA -> evita que los outliers tapen la forma del violín
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.4) +
  
  # facet_wrap(): un panel (faceta) por variable (Glucosa, Presion, Colesterol)
  # scales = "free_y" -> cada variable con su escala de valores independiente
  facet_wrap(~ Variable, scales = "free_y") +
  
  # labs(): etiquetas y título del gráfico
  labs(
    title = "Violin plots por tratamiento - Distribución de todas las variables",
    x = "Tratamiento",
    y = "Valor"
  ) +
  
  # theme_bw(): estilo limpio en blanco y negro
  theme_bw() +
  
  # theme(): ocultamos la leyenda para que el gráfico quede más claro
  theme(legend.position = "none")
# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

# Este gráfico muestra la relación entre los niveles de glucosa (eje X)
# y la presión arterial (eje Y), diferenciando cada punto por el tratamiento.

# Creamos una paleta de colores (una por tratamiento)
# - as.factor(): nos aseguramos de que "Tratamiento" sea un factor
# - RColorBrewer::brewer.pal(): genera colores chulis predefinidos
trat <- as.factor(datos$Tratamiento)
colores <- RColorBrewer::brewer.pal(n = nlevels(trat), name = "Set1")

# Dibujamos el gráfico de dispersión básico con plot()
# - x = Glucosa, y = Presion
# - pch = 19 (punto sólido)
# - col = colores[trat] asigna color según el tratamiento
plot(datos$Glucosa, datos$Presion,
     pch = 19,
     col = colores[trat],
     xlab = "Glucosa",
     ylab = "Presión arterial",
     main = "Relación entre Glucosa y Presión por tratamiento")
# Añadimos la leyenda con legend()
# - "bottomright": ubicación en la esquina inferior derecha
# - legend = levels(trat): texto de la leyenda (en nuestro caso el nombre de los tratamientos)
# - col = colores: colores que corresponden a cada grupo
# - pch = 19: mismo símbolo que en el gráfico
# - title = "Tratamiento": título dentro del recuadro de la leyenda
# - bty = "n": elimina el borde de la caja (opcional, mejora estética)
legend("bottomright",
       legend = levels(trat),
       col = colores,
       pch = 19,
       title = "Tratamiento",
       bty = "n",
       cex = 0.8,        # reduce tamaño del texto
       pt.cex = 0.8)     # reduce tamaño del punto en la leyenda
# GRÁFICO DE DISPERSIÓN: Glucosa vs Presión por tratamiento

# Definimos la variable de tratamiento
trat <- datos$Tratamiento                     # Extraemos la columna "Tratamiento" del data frame

# Asignamos colores a cada grupo de tratamiento
# Usamos tres colores básicos (uno por grupo: FarmacoA, FarmacoB y Placebo)
colores <- c("red", "blue", "green")          # Vector con los colores para cada tratamiento

#Creamos un vector de colores por individuo
# as.numeric(trat) convierte los niveles del factor en números (1, 2, 3)
# Así, cada sujeto recibe el color que le corresponde según su grupo
col_por_trat <- colores[as.numeric(trat)]     

# Dibujamos el gráfico de dispersión (scatter plot)
plot(datos$Glucosa, datos$Presion,            # Eje X = Glucosa, Eje Y = Presión arterial
     pch = 19,                                # Tipo de punto: 19 = círculo sólido
     col = col_por_trat,                      # Color de cada punto según tratamiento
     xlab = "Glucosa",                        # Etiqueta del eje X
     ylab = "Presión arterial",               # Etiqueta del eje Y
     main = "Relación entre Glucosa y Presión por tratamiento")  # Título principal

# Añadimos la leyenda explicando los colores
# legend() añade una caja explicativa para interpretar los colores
legend("topleft",                             # Posición: esquina superior izquierda (mejor visibilidad)
       legend = levels(trat),                 # Texto de la leyenda = nombres de los tratamientos
       col = colores,                         # Usa los mismos colores definidos antes
       pch = 19,                              # Mismo símbolo que en el gráfico
       title = "Tratamiento",                 # Título dentro del cuadro de la leyenda
       bty = "n",                             # Sin borde (box type = "none") para un aspecto más limpio
       cex = 0.9,                             # Tamaño del texto (cex < 1 reduce ligeramente)
       pt.cex = 1.3,                          # Tamaño de los puntos dentro de la leyenda
       text.col = "black",                    # Color del texto (negro)
       inset = c(0.02, 0.02))                 # Deja un pequeño margen desde el borde del gráfico


# Cada punto representa un individuo, coloreado según su tratamiento.
# No se observa una correlación clara entre glucosa y presión arterial.
# Los puntos están bastante dispersos, sin una tendencia lineal evidente.
# Los tratamientos se mezclan, lo que sugiere que el tipo de tratamiento 
# no tiene un efecto fuerte simultáneo sobre ambas variables.
# Hay una ligera tendencia a que valores más altos de glucosa correspondan 
# a presiones algo mayores, pero sin patrón definido.
# La variabilidad es alta en todos los tratamientos.
# En conjunto, los tratamientos parecen producir distribuciones similares 
# en la relación entre glucosa y presión.

# Gráfico de dispersión con ggplot2: Glucosa vs Presión

# Cargamos la librería ggplot2, que permite crear gráficos elegantes y personalizables
library(ggplot2)

# Iniciamos la construcción del gráfico con la función ggplot()
# Definimos los datos (data = datos) y las estéticas (aes):
#  - x = Glucosa → eje horizontal
#  - y = Presion → eje vertical
#  - color = Tratamiento → asigna un color distinto a cada grupo
ggplot(datos, aes(x = Glucosa, y = Presion, color = Tratamiento)) +
  
  # Añadimos los puntos de dispersión (uno por individuo)
  # size = 2 ajusta el tamaño de los puntos (más grandes y visibles)
  geom_point(size = 2) +
  
  # Añadimos una línea de tendencia para cada tratamiento usando un modelo lineal (lm)
  # method = "lm" → ajusta una recta de regresión lineal
  # se = FALSE → oculta el área sombreada del intervalo de confianza
  geom_smooth(method = "lm", se = FALSE) +
  
  # Añadimos etiquetas al gráfico
  # title → título principal
  # x → nombre del eje X
  # y → nombre del eje Y
  labs(title = "Relación Glucosa vs Presión por tratamiento",
       x = "Glucosa", 
       y = "Presión arterial") +
  
  # Aplicamos un tema visual limpio y con fondo blanco
  # theme_bw() (“black & white”) elimina fondos grises y rejillas innecesarias
  theme_bw()

# Cada punto representa un individuo (glucosa vs presión arterial).
# Las líneas rectas son ajustes lineales por tratamiento.
# FarmacoB muestra una ligera tendencia positiva: al aumentar la glucosa, la presión tiende a subir.
# Placebo muestra tendencia negativa: al aumentar la glucosa, la presión tiende a bajar.
# FarmacoA tiene una pendiente casi nula: la presión no cambia con la glucosa.
# Las pendientes son suaves y los puntos dispersos → correlación débil en todos los casos.
# En conclusión: no hay relación clara entre glucosa y presión arterial en los distintos tratamientos.

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
# Un "facet grid" en ggplot2 permite dividir un gráfico en varios paneles
# (subgráficos), normalmente uno por cada categoría (aquí, un panel por tratamiento).
# Es útil para comparar visualmente las relaciones entre variables en distintos grupos.

# Creamos el gráfico
ggplot(datos, aes(x = Colesterol, y = Presion, color = Tratamiento)) +
  
  # geom_point(): dibuja los puntos (cada individuo o muestra)
  geom_point(size = 2, alpha = 0.8) +
  
  # geom_smooth(): añade una línea de tendencia lineal (modelo lm)
  # se = FALSE -> sin sombreado de intervalo de confianza
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  
  # facet_grid(): divide el gráfico en paneles según el tratamiento
  # ". ~ Tratamiento" significa: una fila y varias columnas (una por tratamiento)
  facet_grid(. ~ Tratamiento) +
  
  # Etiquetas y título del gráfico
  labs(title = "Relación Colesterol vs Presión por tratamiento",
       x = "Colesterol",
       y = "Presión arterial") +
  
  # Tema visual limpio
  theme_bw() +
  
  # Ajuste de la leyenda (aquí la ocultamos porque cada panel ya muestra el tratamiento)
  theme(legend.position = "none")

# 7. Realiza un histogramas para cada variable. (0.5 pts)
# Los histogramas muestran cómo se distribuyen los valores de una variable:
# - En el eje X se representan los valores (por ejemplo, de glucosa)
# - En el eje Y se muestra la frecuencia (cuántas observaciones hay en cada rango)
# Así podemos ver si la distribución es normal, sesgada, bimodal, etc.

# Primero reorganizamos los datos en formato "largo" para poder graficar todas las variables a la vez
datos_largo <- datos |>
  pivot_longer(
    cols = all_of(vars_numericas),  # columnas numéricas (Glucosa, Presion, Colesterol)
    names_to = "Variable",          # columna con el nombre de la variable
    values_to = "Valor"             # columna con los valores numéricos
  )

# Creamos el gráfico de histogramas
ggplot(datos_largo, aes(x = Valor, fill = Variable)) +
  
  # geom_histogram(): crea los histogramas
  # bins = 30 define el número de barras (intervalos)
  # alpha = 0.8 controla la transparencia para que los colores no tapen todo
  geom_histogram(bins = 30, alpha = 0.8, color = "black") +
  
  # facet_wrap(): crea un panel para cada variable numérica
  # scales = "free" -> cada variable puede tener su propia escala de valores
  facet_wrap(~ Variable, scales = "free") +
  
  # labs(): añade título y etiquetas
  labs(
    title = "Distribución de valores - Histogramas por variable",
    x = "Valor medido",
    y = "Frecuencia"
  ) +
  
  # theme_bw(): aplica fondo blanco con bordes limpios
  theme_bw() +
  
  # theme(): eliminamos la leyenda (ya tenemos los nombres en los paneles)
  theme(legend.position = "none")

# Los histogramas muestran la distribución de cada variable biomédica.
# Cada panel representa una variable: Colesterol, Glucosa y Presión arterial.
# Las tres distribuciones son aproximadamente normales (simétricas y acampanadas).
# No se observan valores atípicos extremos ni sesgos marcados.
# Glucosa es la variable más concentrada (menos dispersión entre individuos).
# Presión y Colesterol muestran mayor variabilidad.
# Las medias se sitúan en torno a 200 (colesterol), 105 (glucosa) y 125 (presión).
# En conjunto, los datos presentan una distribución adecuada para análisis paramétricos (como ANOVA).


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
# En R, un "factor" es una variable categórica (no numérica) que
# tiene un conjunto limitado de valores llamados *niveles*.
# Ejemplo: "Tratamiento" puede tener 3 niveles -> "FarmacoA", "FarmacoB", "Placebo".
# Los factores se usan en análisis estadísticos y gráficos para agrupar datos por categorías.

# Primero comprobamos cómo está actualmente la variable Tratamiento
str(datos$Tratamiento)

# Si no es un factor (por ejemplo, aparece como 'character' o 'text'),
# la convertimos a factor con as.factor()
datos$Tratamiento <- as.factor(datos$Tratamiento)

# Volvemos a comprobar para confirmar el cambio
str(datos$Tratamiento)

# También podemos ver los niveles que contiene el factor
levels(datos$Tratamiento)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Usaremos la función aggregate() de base R.
# Su sintaxis es:
#   aggregate( VARIABLE ~ AGRUPADOR, data = DATOS, FUNCIÓN )
# En este caso: queremos aplicar una función personalizada (mean y sd)
# a la variable Glucosa agrupada por Tratamiento.

# Versión con aggregate():
resumen_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos, 
                             FUN = function(x) c(Media = mean(x), SD = sd(x)))

# Mostramos el resultado
print(resumen_glucosa)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

# Extraemos las filas correspondientes a cada tratamiento:
placebo  <- subset(datos, Tratamiento == "Placebo")
farmacoA <- subset(datos, Tratamiento == "FarmacoA")
farmacoB <- subset(datos, Tratamiento == "FarmacoB")

# Comprobamos cuántas filas tiene cada subconjunto
nrow(placebo)
nrow(farmacoA)
nrow(farmacoB)



# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

# El test de Shapiro-Wilk (shapiro.test) evalúa si una variable sigue una distribución normal.
# H0 (hipótesis nula): los datos son normales
# Si p > 0.05 → no se rechaza H0 → distribución normal
# Si p < 0.05 → se rechaza H0 → los datos NO son normales

# Aplicamos el test de Shapiro para cada tratamiento:
by(datos$Glucosa, datos$Tratamiento, shapiro.test)

# Los resultados del test de Shapiro-Wilk muestran p-valores > 0.05 en todos los tratamientos.
#  No se rechaza la hipótesis nula de normalidad.
# Por tanto, los datos de glucosa se distribuyen aproximadamente de forma normal
# en los tres grupos: Placebo, FarmacoA y FarmacoB.

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
# Requisitos previos:
# - La variable Glucosa debe seguir una distribución normal (ver punto 11)

anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)

# Mostramos los resultados
summary(anova_glucosa)
# Antes de aplicar el análisis, se comprobó el supuesto de normalidad mediante
# la prueba de Shapiro-Wilk, obteniendo p-valores superiores a 0.05 en todos los grupos.
# Esto indica que las distribuciones de glucosa son aproximadamente normales
# y que el uso del ANOVA es adecuado.

# El resultado del ANOVA fue:
# F(2, 97) = 2.36, p = 0.10

# Dado que el valor de p es mayor que 0.05, no se rechaza la hipótesis nula.
# Por tanto, no existen diferencias estadísticamente significativas
# entre las medias de glucosa de los distintos tratamientos.

# En consecuencia, los tratamientos no parecen modificar de forma relevante
# los niveles medios de glucosa en la muestra analizada.
# Las pequeñas variaciones observadas entre grupos se atribuyen
# a la variabilidad natural de los datos y no a un efecto del tratamiento.
# Boxplot de glucosa por tratamiento con media y dispersión
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "ANOVA - Niveles de Glucosa por Tratamiento",
       x = "Tratamiento", y = "Glucosa") +
  theme_bw()

