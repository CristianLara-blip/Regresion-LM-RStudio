# Instalar y cargar las librerías necesarias
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("caret")

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)

# Importar datos
df <- read_excel("G:/datos_producción-oficial.xlsx")

# Mostrar las primeras filas para verificar la carga de datos
print(head(df, 2))

# Eliminar el símbolo "°C" de la columna "Temperatura Ambiente" y convertir a numérico
df$`Temperatura Ambiente` <- as.numeric(gsub("°C", "", df$`Temperatura Ambiente`))

# Seleccionar variables
variables_x <- c("Horas Trabajadas", "Horas Descanso", "Temperatura Ambiente", "Cantidad de Trabajadores")
variable_y <- "Productos Terminados"

# Crear y entrenar el modelo de regresión lineal
modelo <- lm(`Productos Terminados` ~ ., data = df[, c(variables_x, variable_y)])

# Mostrar los coeficientes del modelo
print(summary(modelo)$coefficients)

# Mostrar modelo completo
summary(modelo)

# Imprimir la ecuación del modelo
coeficientes <- summary(modelo)$coefficients
ecuacion <- paste("y =", paste(round(coeficientes[-1, 1], 3), variables_x, sep = " * ", collapse = " + "), "+", round(coeficientes[1, 1], 3))
print(paste("Ecuación del modelo: ", ecuacion))

# Calcular y mostrar el coeficiente de determinación (R^2)
r2 <- summary(modelo)$r.squared
print(paste("Coeficiente de determinación (R^2): ", round(r2, 3)))

# Gráfico de Importancia de las Características (Coeficientes)
importancia <- data.frame(Variable = variables_x, Importancia = coef(modelo)[-1])
ggplot(importancia, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importancia (Coeficientes)") +
  ggtitle("Importancia de cada variable en el modelo de regresión") +
  theme_minimal()

# Mapa de Calor de Correlaciones
correlaciones <- cor(df[, c(variables_x, variable_y)])
melted_correlaciones <- melt(correlaciones)
ggplot(data = melted_correlaciones, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  ggtitle("Mapa de Calor de Correlaciones")

# Generar predicción con nuevas variables
nuevas_variables <- data.frame(
  `Horas Trabajadas` = 12,
  `Horas Descanso` = 2,
  `Temperatura Ambiente` = 30,
  `Cantidad de Trabajadores` = 5
)

# Asegurarse de que los nombres de las columnas coincidan con los nombres en el modelo
names(nuevas_variables) <- variables_x

# Realizar la predicción
prediccion_nueva <- predict(modelo, nuevas_variables)

# Mostrar la predicción
print(paste("La predicción de productos terminados para las nuevas variables es:", round(prediccion_nueva, 1)))

