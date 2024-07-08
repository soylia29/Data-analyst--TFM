#### DATOS GRADO ####
library(readxl)
educ <-read_excel("C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/educgrado.xlsx")
View(educgrado)
summary(educ$`% Mujeres`)
educgrados <- educ

# Verificar el tipo de datos de la columna
class(educgrados$`% Mujeres`)

# Remover comas y otros caracteres no numéricos
educgrados$`% Mujeres` <- gsub("[^0-9.]", "", educgrados$`% Mujeres`)

# Convertir la columna a numérica
educgrados$`% Mujeres` <- as.numeric(educgrados$`% Mujeres`)

# Verificar el tipo de datos después de la conversión
class(educgrados$`% Mujeres`)
summary(educgrados)

### Valor incial y final grados ####

# Paso 1: Encontrar el primer año con matriculados para cada titulación


library(dplyr)
Matriculados_2015_2016 <- educgrados %>%
  filter(Curso == "2015-2016") %>%
  group_by(Titulación) %>%
  summarise(Matriculados_2015_2016 = mean(Matriculados)) %>%
  ungroup()

#Unir datos

# Filtrar las filas de educgrados donde el Curso no es '2015-2016'

library(dplyr)
educgrados_filtrado <- educgrados %>% 
  filter(Curso != '2015-2016')

# Realizar la unión después de filtrar
datos_completos <- left_join(Matriculados_2015_2016, educgrados_filtrado, by = "Titulación")

colnames(datos_completos)

datos_completos$DiferenciaMatriculados_Relacion2015_2016<- datos_completos$Matriculados - datos_completos$Matriculados_2015_2016



datos_completos$Porcentaje_Crecimiento <- ((datos_completos$Matriculados- datos_completos$Matriculados_2015_2016) / datos_completos$Matriculados_2015_2016)


 
write_xlsx(datos_completos, "C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/datoscompletosGrado.xlsx")


#### KMEANS ####

library(ggplot2)

# Seleccionar las columnas relevantes para el clustering
datos_clustering <- datos_completos[, c("Matriculados_2015_2016", "Matriculados")]

# Normalizar los datos para que tengan una escala similar
datos_normalizados <- scale(datos_clustering)

# Especificar el número de clústeres que deseas encontrar
k <- 3

# Ejecutar el algoritmo K-means
kmeans_result <- kmeans(datos_normalizados, centers = k)



# Graficar los grupos identificados por kmeans
plot(datos_completos[, c("Matriculados_2015_2016", "Matriculados")], 
     col = kmeans_result$cluster, 
     pch = 20, 
     main = "Grupos identificados por k-means",
     xlab = "Matriculados_2015_2016",
     ylab = "Matriculados anualmente")

# Agregar leyenda
legend("topright", legend = unique(kmeans_result$cluster), col = unique(kmeans_result$cluster), pch = 20)

     

# Agregar los centroides de los grupos al gráfico
points(kmeans_result$centers[, c("Matriculados_2015_2016", "Matriculados")], 
       col = 1:3, 
       pch = 4, 
       cex = 2)



