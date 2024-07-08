library(readxl)
X2023_Estad_EEU_MatriculadosEEU <- read_excel("C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/2023_Estad_EEU-MatriculadosEEU.xlsx"col_names = FALSE)
View(X2023_Estad_EEU_MatriculadosEEU)

X2023_Estad_EEU_MatriculadosEEUgrado <- read_excel("C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/2023_Estad_EEU-MatriculadosEEU.xlsx", col_names = FALSE)

library(tidyr)
library(readxl)
library(dplyr)
library(writexl)


educgrado <- X2023_Estad_EEU_MatriculadosEEUgrado
col_names <- educgrado[4, ]
col_namestoadd <- educgrado[3, ]
educgrado <- educgrado[-(1:2),]

for (i in 1:nrow(col_namestoadd)) {
  for (j in 2:ncol(col_namestoadd)) {
    if (is.na(col_namestoadd[i, j])) {
      col_namestoadd[i, j] <- col_namestoadd[i, j - 1]
    }
  }
}


print(col_namestoadd)

col_namestoadd[is.na(col_namestoadd)] <- ""


merged_cols <- data.frame(matrix(ncol = 20, nrow = 1))

for (i in 1:20) {
  if (col_namestoadd[[1, i]] == "") {
    merged_cols[[1, i]] <- col_names[[1, i]]
  } else {
    if (col_names[[1, i]] != "") {
      merged_cols[[1, i]] <- paste0(col_names[[1, i]], " ", col_namestoadd[[1, i]])
    } else {
      merged_cols[[1, i]] <- col_namestoadd[[1, i]]
    }
  }
}


print(merged_cols)

colnames(educgrado) <- merged_cols

educgrado <- educgrado[-(1:2),]

educgrado[educgrado == "-"] <- 0

educgrado[, -c(1:4)] <- lapply(educgrado[, -c(1:4)], as.numeric)

# Reemplazar NA por 0 en todo el data frame
educgrado[is.na(educgrado)] <- 0

educgrado <- educgrado %>%
  pivot_longer(cols = -c("Comunidad autónoma", "Universidad", "Rama", "Titulación"), 
               names_to = c(".value", "Curso"),
               names_pattern = "(\\D+) (\\d+-\\d+)",
               values_to = c("Matriculados", "% Mujeres"))


write_xlsx(educgrado, "C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/educgrado.xlsx")
```
