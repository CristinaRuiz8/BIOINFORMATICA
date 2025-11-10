# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
# Podemos instalarlos con la primera forma:
if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install(c("gplots","RColorBrewer"))

# O con la segunda
install.packages("gplots")		
install.packages("RColorBrewer")	

# Cargamos los paquetes
library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
+ col = redgreen(75), scale = "row",labRow=FALSE)