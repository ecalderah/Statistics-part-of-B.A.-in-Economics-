#-------------------------------   Tarea 18 ---------------------------------#
# Cargas las librerias necesarias. Si no las tienes: installpackages("...")

library(alphavantager) # Ésta es la API
library(ggplot2)       # Gráficos avanzados

#--------------------  Obtener los datos de a la Api  ------------------------#

# Configurar la API key de la pagina oficial
av_api_key("T33J841II4TUKAZJ")

# Definir el instrumento y las variables de fechas
symbol <- "AMX"  # Aqui cambias el instrumento financiero
start_date <- as.Date(Sys.Date() - 5*365)  
end_date <- Sys.Date()

# Descargar los datos completos
df <- av_get(symbol = symbol, 
             av_fun = "TIME_SERIES_DAILY", 
             outputsize = "full")

# filtrar los datos por fecha.
df <- df[df$timestamp >= start_date & df$timestamp <= end_date, ]

#----------------------   Calcular ln(R_t/R_t-1)  ---------------------------#
  #-----------------------  = lnR_t - lnR_t-1  ---------------------------#

# Se calcula el logaritmo del precio de cierre ('close') y se obtiene la diferencia
df$log_return <- c(NA, diff(log(df$close)))

# Eliminar la primera fila que contiene NA para evitar problemas con la libreria
df <- df[!is.na(df$log_return), ]

#-------------------------------- Graficar  ----------------------------------#

# Asignamos los valores a graficar y el rango del eje x
p_hist <- ggplot(df, aes(x = log_return)) +
  
  # Histograma
  geom_histogram(aes(y = ..density..), bins = 50, 
                 fill = "lightblue", color = "black", alpha = 0.7) +
  
  # Densidad normal
  stat_function(fun = dnorm, 
                args = list(mean = mean(df$log_return), 
                            sd = sd(df$log_return)),
                color = "red", size = 0.5) +
  
  # Etiquetas y título
  labs(title = paste("Histograma del", symbol),
       x = expression(ln(P[t]/P[t-1])),
       y = "Densidad") +
  theme_minimal()

# Mostrar la grafica
print(p_hist)


