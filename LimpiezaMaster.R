library(readxl)
X2023_Estad_EEU_MatriculadosEEUmaster <- read_excel("C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/2023_Estad_EEU-MatriculadosEEU.xlsx", 
                                                    sheet = "Matriculados Master", col_names = FALSE)
View(X2023_Estad_EEU_MatriculadosEEUmaster)



# parte 1 
educmaster <- X2023_Estad_EEU_MatriculadosEEUmaster
col_names <- educmaster[4, ]
col_namestoadd <- educmaster[3, ]
educmaster <- educmaster[-(1:2),]

# parte 2

for (i in 1:nrow(col_namestoadd)) {
  for (j in 2:ncol(col_namestoadd)) {
    if (is.na(col_namestoadd[i, j])) {
      col_namestoadd[i, j] <- col_namestoadd[i, j - 1]
    }
  }
}


print(col_namestoadd)

# parte 3
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

colnames(educmaster) <- merged_cols

educmaster <- educmaster[-(1:2),]

# Parte 4

educmaster[, -c(1:4)] <- lapply(educmaster[, -c(1:4)], as.numeric)

# Reemplazar NA por 0 en todo el data frame
educmaster[is.na(educmaster)] <- 0


# Parte 5

educmaster <- educmaster %>%
  pivot_longer(cols = -c("Comunidad autónoma", "Universidad", "Rama", "Titulación"), 
               names_to = c(".value", "Curso"),
               names_pattern = "(\\D+) (\\d+-\\d+)",
               values_to = c("Matriculados", "% Mujeres")) 

write_xlsx(educmaster, "C:/Users/Lenovo/OneDrive/Escritorio/MASTER/TFM/PROGRESO/educmaster.xlsx")
