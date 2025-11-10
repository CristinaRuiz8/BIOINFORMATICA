#############################################################################
#
# PRACTICA R
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data) 
head(data) #nos saca las 6 primeras filas
tail(data) #6 últimas filas

# Hacemos un primer histograma para explorar los datos
par(mar = c(4, 4, 2, 1))  # abajo, izquierda, arriba, derecha

hist(data, col = "pink", main="GSE5583 - Histogram")

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
# Sirve para mejorar la visualización de los datos, 
# Los datos biológicos suelen estar muy dispersos,con valores muy pequeños y otros extremadamente grandes.
# La escala logarítmica "comprime" los valores grandes y "expande" los pequeños,
# haciendo la distribución más simétrica.
# el histograma se observa más visual
data2 = log2(data)
hist(data2, col = "red", main="GSE5583 (log2) - Histogram")


# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
#Un boxplot es una representación gráfica que resume la distribución de un conjunto de datos numéricos.
#Mediana (línea dentro de la caja).
#Cuartiles (Q1 = 25%, Q3 = 75% → los bordes de la caja).
#Rango intercuartílico (IQR = Q3 - Q1) → la altura de la caja.
#Bigotes (whiskers) → se extienden desde los cuartiles hasta 1.5 × IQR o hasta el valor más extremo dentro de ese rango.
#Outliers (puntos fuera de los bigotes).
#Nos saca un boxplot para cada columna, ha puesto el titulo llamado GSE5583-boxplots. 
#Las=2 sirve para que aparezcan los nombres en vertical 
#col=Define los colores de las cajas del boxplot.
boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)
	
boxplot(data, col=c("blue", "blue", "blue",
                     "orange", "orange", "orange"),
        main="GSE5583 - boxplots", las=2)
#sin transformar no queda bien 
# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación ç
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist(1-cor(data2)))
plot(hc, main="GSE5583 - Hierarchical Clustering")
#clustering es para comprobar que hemos conseguido las muestras bien, con patrones similares. 
#Nos dice si hemos cogido muestras con genes similares, nos agrupa WT  en un cluster y KO en otro cluster por lo tanto es correcto.

#######################################
# Análisis de Expresión Diferencial 
#######################################
# tenemos un listado de genes con expresiones de KO y WT queremos ver que genes son suficientemente distintos para 
# ver cuales tienen efecto, o un papel importante en la expresión de ese gen
# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
# una variable WT con 3 variables suyas y una variable KO con sus tres variables
#un subconjunto de la matriz original, que contiene únicamente las 3 muestras de la condición Wild Type (control) y lo mismo con el KO
head(data)
wt <- data[,1:3]
ko <- data[,4:6]
class(wt)
head(wt)
head(ko)
#hemos creado una matriz de datos
# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean)
ko.mean = apply(ko, 1, mean)
head(wt.mean)
head(ko.mean)

# ¿Cuál es la media más alta?
limit = max(wt.mean, ko.mean)
limit
# la media mas alta es 37460.5
# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO",
	main = "GSE5583 - Scatter", xlim = c(0, limit), ylim = c(0, limit))
# Añadir una línea diagonal con abline
abline(0, 1, col = "red")
# si la correlación fuese perfecta estarian pegado a esta linea
# ¿Eres capaz de añadirle un grid?
grid()
#abline(a, b): línea de pendiente b y ordenada en el origen a
#abline(h=y): linea horizontal
#abline(v=x): línea vertical
abline(1, 2, col = "red")     
abline(h = 2, col = "green") 
abline(v = 3, col = "violet") 
# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean

# Hacemos un histograma de las diferencias de medias
hist(diff.mean, col = "gray")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué? porque para analizar los datos es mejor usa los rawdata porque es más preciso
# ¿Cuántas valores tiene cada muestra?
nrow(data)   # número de genes = valores por muestra tiene 12488
ncol(data)   # número de muestras es 6
#con un bucle for para cada gen de la tabla crea un vector x que tenga los tres valores en WT y los tres de KO. 
pvalue = NULL 
tstat = NULL 

for(i in 1 : nrow(data)) { #Para cada gen
	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i
	
	# Hacemos el test t de student y la guardamos en una vaiable
	t = t.test(x, y)
	
	# Añadimos el p-value a la lista. Saca los p-values y lo añadimos a la variable
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}
#dice que le muestres los primeros
head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue)
#como no hemos valuado la normalidad no sabemos si tiene una distribución normal o no.
#por lo tanto la t-student no es muy concluyente
# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
#Trasnformamos los datos, ahora un logaritmo de base 2, da igual si es 10 o 2 y sirve para visualizar los datos mejor.
hist(pvalue,col="gray")
hist(-log10(pvalue), col = "gray")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")
#un volcano plot es un gráfico que representa la diferencia de media con los p values trasnformados. Las pelotitas son los genes. 
# Un volcano plot es un gráfico de dispersión que representa:
# En el eje X: la diferencia de expresión entre condiciones 
# (log2 fold change, es decir log2(WT/KO)).
# En el eje Y: la significación estadística, normalmente –log10(p-value).
# Si el valor en X ≈ 0 → el gen se expresa igual en WT y KO (sin diferencia).
# Si el valor en X > 0 → el gen está más expresado en WT.
# Si el valor en X < 0 → el gen está más expresado en KO.
# Cuanto más alto está el punto (eje Y), más significativo es el resultado (menor p-value).
# ¿Puedes representarlo en el gráfico?
# ha hecho un filtro de media y un filtro de p-value y el corte d p-value =0,01
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)

#abline(v = -diff.mean_cutoff, col = "red", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
#filter nos muestra cuantos gene estan expesados
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff

dim(data[filter_by_diff.mean, ])

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
#426 genes cumplen los do criterios
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")

# en la grafica sale al reves porq la sobreexpresion es con valores negativos y represión pcon valores positivos porque lo hemos decidido asi al principio.

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# filtered: la matriz de datos que queremos visualizar (genes en filas, muestras en columnas).
# Rowv=rowv: dendrograma que ordena las filas (genes) según su similitud (clustering jerárquico).
# Colv=colv: dendrograma que ordena las columnas (muestras) según su similitud.
# cexCol=0.7: ajusta el tamaño de las etiquetas de las columnas (más pequeño para que quepan).
# labRow=FALSE: elimina las etiquetas de las filas (genes), ya que son demasiados y saturarían la figura.

# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE, col=hcl.colors(50, "RdBu"))

heatmap(filtered) #sin ordenar por genes
#es un ejemplo de clustering. Donde le metes todas las columnas y filas y las clasifica en su sitio. Todas las eplicas de WT en un bloque y las de kO en otro bloque. Solo se hacen con los genes diferencialmente expresados. Si esta sobreexpresado en WT esta reprimido en Ko y al revés.
#los arboles de arriba son el clustering

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")
#rojos es sobreexpresión y azul, reprimido
# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
