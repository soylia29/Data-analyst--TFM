#### DATOS MASTER ####
library(readxl)
educ <-read_excel("C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/educmaster.xlsx")
View(educmaster)
summary(educ$`% Mujeres`)
educmaster <- educ

# Verificar el tipo de datos de la columna
class(educmaster$`% Mujeres`)

# Remover comas y otros caracteres no numéricos
educmaster$`% Mujeres` <- gsub("[^0-9.]", "", educmaster$`% Mujeres`)

# Convertir la columna a numérica
educmaster$`% Mujeres` <- as.numeric(educmaster$`% Mujeres`)

# Verificar el tipo de datos después de la conversión
class(educmaster$`% Mujeres`)
summary(educmaster)

### Valor incial y final grados ####

# Paso 1: Encontrar el primer año con matriculados para cada titulación


library(dplyr)
Matriculados_2015_2016 <- educmaster %>%
  filter(Curso == "2015-2016") %>%
  group_by(Titulación) %>%
  summarise(Matriculados_2015_2016 = mean(Matriculados)) %>%
  ungroup()

#Unir datos

# Filtrar las filas de educgrados donde el Curso no es '2015-2016'

library(dplyr)
educmaster_filtrado <- educmaster %>% 
  filter(Curso != '2015-2016')

# Realizar la unión después de filtrar
datos_completosmaster <- left_join(Matriculados_2015_2016, educmaster_filtrado, by = "Titulación")

colnames(datos_completosmaster)

datos_completosmaster$DiferenciaMatriculados_Relacion2015_2016<- datos_completosmaster$Matriculados - datos_completosmaster$Matriculados_2015_2016



datos_completosmaster$Porcentaje_Crecimiento <- ((datos_completosmaster$Matriculados- datos_completosmaster$Matriculados_2015_2016) / datos_completosmaster$Matriculados_2015_2016)



write_xlsx(datos_completosmaster, "C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/datoscompletosMaster.xlsx")


#### KMEANS ####

library(ggplot2)

# Seleccionar las columnas relevantes para el clustering
datos_clustering <- datos_completosmaster[, c("Matriculados_2015_2016", "Matriculados")]

# Normalizar los datos para que tengan una escala similar
datos_normalizados <- scale(datos_clustering)

# Especificar el número de clústeres que deseas encontrar
k <- 3

# Ejecutar el algoritmo K-means
kmeans_result <- kmeans(datos_normalizados, centers = k)



# Graficar los grupos identificados por kmeans
plot(datos_completosmaster[, c("Matriculados_2015_2016", "Matriculados")], 
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