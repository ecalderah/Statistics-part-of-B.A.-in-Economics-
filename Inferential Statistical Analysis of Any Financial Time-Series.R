#=========================       Proyecto Final         ========================#
#=========================    Estadística Inferencial   ========================#
#-------------------------   Universidad de Guanajuato  ------------------------#

##--------------------------     Configuración          ------------------------####

# Primero cargamos las librerias necesarias. Si no se tienen: installpackages("...")

  library(alphavantager) # Ésta es la API
  library(ggplot2)       # Gráficos avanzados
  library(tidyquant)     # extiende ggplot2 con geometrías financieras

#--------------------  Obtener los datos de a la Api  ------------------------#

  # Configurar la API key de la pagina oficial
  av_api_key("T33J841II4TUKAZJ")
  
  # Definir el instrumento y las variables de fechas
  symbol <- "NVDA"  # Aqui asignamos el instrumento financiero

  # Descargar los datos completos de los activos financieros
  df <- av_get(symbol = symbol, 
               av_fun = "TIME_SERIES_DAILY", 
               outputsize = "full")

##=============================== Problema 1            =============================####
#--------------------------   Calcular y graficar  ---------------------------#

  # X_t= (P_t-P_t-1)/P_t-1)                                                      #
  # Y_t = lnR_t - lnR_t-1                                                        #
  
  #------------------- 1. Calcular retornos ---------------------------#
  df$Y_t <- c(NA, diff(log(df$close)))   # Y_t = ln(P_t) - ln(P_{t-1})
  df$X_t <- c(NA, diff(df$close) / head(df$close, -1)) # X_t = P_t - P_{t-1} / P_{t-1}
  
  # Esto crea 2 nuevas columnas con un elemento inicial con nada (NA) lo eliminamos:
  
  df <- df[!is.na(df$X_t) & !is.na(df$Y_t), ]
  #                  ^                ^
  #        ambas condiciones unidas con &
  
##  Gráfico 1 : P_t                                     ---------------------------------------------------------
  #   1) library(tidyquant) ya cargada podemos hacer un gráfico de candlesticks para P_t
  
  ggplot(df, aes(x = timestamp,   # Asignamos los datos
                 open  = open,    #  correspondientes a la libreria e información bursatil
                 high  = high,
                 low   = low,
                 close = close)) +
    geom_candlestick() +                     # velas japonesas
    theme_tq() +                             # tema predeterminado de tidyquant
    labs(title   = symbol,
         x = NULL, y = NULL)   

##   2) Para graficar X_t y Y_t con lineas: 

##  Gráfico 2 : X_t                                     ---------------------------------------------------------
  
  ggplot(df, aes(timestamp, X_t)) +             # Grafico basico de lineas
    geom_line(linewidth = .3, colour = "steelblue") +
    theme_tq() +
    labs(title = "Retorno relativo  (X_t)", x = NULL, y = NULL)

##  Gráfico 3 : Y_t                                     ---------------------------------------------------------

    ggplot(df, aes(timestamp, Y_t)) +             # Grafico basico de lineas
    geom_line(linewidth = .3, colour = "Red") +
    theme_tq() +
    labs(title = "Log-retorno         (Y_t)", x = NULL, y = NULL)

##=============================== Problema 2            =============================####
##  Inciso a                                            ------------------------------------------------------------
  
  # Medias muestrales
  mu_X_hat     <- mean(df$X_t)
  mu_Y_hat     <- mean(df$Y_t)
  
  # Varianzas muestrales insesgadas
  sigma2_X_hat <- var(df$X_t)   # var() usa 1/(n-1)
  sigma2_Y_hat <- var(df$Y_t)
  
  # Mostramos los resultados
  c(mu_X_hat   = mu_X_hat,
    sigma2_X   = sigma2_X_hat,
    mu_Y_hat   = mu_Y_hat,
    sigma2_Y   = sigma2_Y_hat)

##  Inciso b                                            ---------------------------------------------------------
  
  # Calculamos σ̂ (desviación estándar) a partir de la varianza
  sigma_X_hat <- sqrt(sigma2_X_hat)                           # σ̂_X = √var
  sigma_Y_hat <- sqrt(sigma2_Y_hat)                           # σ̂_Y = √var
  
  # Dibujamos la densidad normal de X_t
  curve(dnorm(x, mu_X_hat, sigma_X_hat),                # densidad N(μ̂_X,σ̂_X
        from = mu_X_hat - 4*sigma_X_hat,                # rango ±4 σ̂
        to   = mu_X_hat + 4*sigma_X_hat,
        n    = 1000, col = "steelblue", lwd = 2,
        xlab = "", ylab = "",
        main = "Densidades normales de X_t y Y_t")
  
  # Añadimos la densidad normal de Y_t sobre la misma figura
  curve(dnorm(x, mu_Y_hat, sigma_Y_hat),                # densidad N(μ̂_Y,σ̂_Y)
        add  = TRUE, col = "firebrick", lwd = 2, lty = 3)
  
  # Leyendas y etiquetas
  legend("topright", legend = c("X_t", "Y_t"),
         col = c("steelblue", "firebrick"), lwd = 2, bty = "n")

##=============================== Problema 3            =============================####
##  Inciso a b y c                                      ---------------------------------------------------

  bins_vec <- c(10, 20, 30)                # asignamos las k clases
  vars     <- c("X_t", "Y_t")              # variables a graficar
  
  # medias y sigmas ya calculadas en el Problema 2
  sigmas <- c(X_t = sqrt(sigma2_X_hat),
              Y_t = sqrt(sigma2_Y_hat))
  mus    <- c(X_t = mu_X_hat,
              Y_t = mu_Y_hat)
  
  for (v in vars) {         # Para no hacer tanto codigo, Iteramos la clases con las variables 
    for (k in bins_vec) {   # para hacer una grafica de densidad normal de X_t y Y_t en cada clase
      
      ggplot(df, aes(.data[[v]])) +
        geom_histogram(aes(y = after_stat(density)),  # histograma de frecuencia relativa
                       bins = k, #Clases
                       fill = "grey70", colour = "white") +
        stat_function(fun  = dnorm,                    # curva normal
                      args = list(mean = mus[v], sd = sigmas[v]),# mu y sigmas
                      colour = "firebrick", linewidth = 1) +
        theme_tq() +
        labs(title = sprintf("%s – %d clases", v, k),
             x = NULL, y = "Frecuencia relativa") -> p
      
      print(p)   # muestra el gráfico en el visor
    }
  }

##=============================== Problema 4            =============================####
##  Resultados de las probabilidades para X_t           -------------
  r <- df$X_t ;  mu <- mean(r);  s <- sd(r)
  
  ## 2. Función auxiliar para Φ(a,b) = P(a < Z ≤ b) = Φ(b)−Φ(a).

  phi <- function(a,b) pnorm(b,mu,s) - pnorm(a,mu,s)
  
  ## 3. Tabla
  res <- rbind(
    Emp = c(mean(r>0.01),
            mean(r>0 & r<0.01),
            mean(r>0 & r<=0.01),
            mean(r>0.01 & r<=0.5),
            mean(r>0.5 & r<=1),
            mean(r<=0)),
    Norm = c(1-pnorm(0.01,mu,s),
             phi(0,0.01),
             phi(0,0.01),          # mismo intervalo abierto/cerrado
             phi(0.01,0.5),
             phi(0.5,1),
             pnorm(0,mu,s))
  ); colnames(res) <- c(">0.01","0<r<0.01","0<r≤0.01",
                        "0.01<r≤0.5","0.5<r≤1","r≤0");  res
  
##  Resultados de las probabilidades para Y_t           -------------
  
  ## ── Probabilidades para Y_t en el mismo estilo que la primera tabla ──────────
  y  <- df$Y_t;   mu <- mean(y);   s <- sd(y)
  
  ## Φ(a,b) = P(a < Z ≤ b) para Z ~ N(mu, s²)
  phi <- function(a,b) pnorm(b, mu, s) - pnorm(a, mu, s)
  
  ## Matriz 2 × 6  (fila 1 = empírico, fila 2 = normal)
  
  res_Y <- rbind(
    Emp = c(mean(y <= 0),
            mean(y > 0      & y <  0.001),
            mean(y > 0.001),
            mean(y > 0      & y <= 0.001),          # mismo intervalo cerrado
            mean(y > 0.001  & y <= 0.0487),
            mean(y > 0.0487 & y <= 0.1397)),
    Norm = c(pnorm(0,       mu, s),
             phi(0,         0.001),
             1 - pnorm(0.001, mu, s),
             phi(0,         0.001),                 
             phi(0.001,     0.0487),
             phi(0.0487,    0.1397))
  )
  
  colnames(res_Y) <- c("Y≤0",
                       "0<Y<0.001",
                       "Y>0.001",
                       "0<Y≤0.001",
                       "0.001<Y≤0.0487",
                       "0.0487<Y≤0.1397")
  
  res_Y   # muestra la tabla
  
##=============================== Problema 5            =============================####
##  Tabla                                               ####  
  alpha  <- c(0.005, 0.001, 0.0001)                                  # tres alfas
  kX <- sum(df$X_t > 0.005 & df$X_t < 0.01)                    # éxitos X_t
  kY <- sum(df$Y_t > 0.005 & df$Y_t < 0.01)                    # éxitos Y_t
  n  <- nrow(df)                       # tamaño muestral (total de observaciones)
  
  out <- t(sapply(alpha, \(al){                                    # p̂  +  IC exacto
    ciX <- binom.test(kX, n, conf.level = 1 - al)$conf.int
    ciY <- binom.test(kY, n, conf.level = 1 - al)$conf.int
    c(alpha = al, p_hat_X = kX/n, ciX, p_hat_Y = kY/n, ciY)
  }))
  
  colnames(out) <- c("alpha","P[A]","LI_X","LS_X","P[C]","LI_Y","LS_Y")
  out
  
##=============================== Problema 6            =============================####
## Inciso a y b                                         ####
  
  # --- 1 · Separar conjunto histórico y conjunto de validación (últimas 4 semanas)
  cut <- max(df$timestamp) - lubridate::weeks(4)   # fecha de corte 28 días antes del último dato
  h   <- df[df$timestamp <  cut, ]                 # datos hasta la fecha de corte (histórico)
  l   <- df[df$timestamp >= cut, ]                 # datos desde la fecha de corte (hold–out de 4 semanas)
  
  # --- 2 · Definir función para intervalos exactos (Clopper–Pearson) con nivel de confianza 1 – α
  f <- function(k, n, a) 
    binom.test(k, n, conf.level = 1 - a)$conf.int   # devuelve vector [LI, LS] para p = k/n
  
  # --- 3 · Calcular conteos históricos y proporciones puntuales con sus IC
  kA <- with(h, sum(X_t > 0.005 & X_t < 0.01))   # número de éxitos en evento A dentro del histórico
  kC <- with(h, sum(Y_t > 0.005 & Y_t < 0.01))   # número de éxitos en evento C dentro del histórico
  n  <- nrow(h)                                  # tamaño de la muestra histórica (número de observaciones)
  
  ci <- t(
    sapply(alpha, \(a) 
           c(
             kA / n,         # proporción puntual de A en el histórico
             f(kA, n, a),    # límites inferior y superior del IC para p_A al nivel 1 – a
             kC / n,         # proporción puntual de C en el histórico
             f(kC, n, a)     # límites inferior y superior del IC para p_C al nivel 1 – a
           )
    )
  )
  colnames(ci) <- c(
    "P[A]", "LI_P[A]", "LS_P[A]",  # proporción puntual e IC para A
    "P[C]", "LI_P[C]", "LS_P[C]"   # proporción puntual e IC para C
  )
  
  # --- 4 · Calcular éxitos y frecuencias en las últimas 4 semanas (hold–out)
  kA4 <- with(l, sum(X_t > 0.005 & X_t < 0.01))   # éxitos del evento A en el conjunto hold–out
  kC4 <- with(l, sum(Y_t > 0.005 & Y_t < 0.01))   # éxitos del evento C en el conjunto hold–out
  n4  <- nrow(l)                                  # tamaño de la muestra hold–out
  pA4 <- kA4 / n4                                 # frecuencia relativa de A en hold–out
  pC4 <- kC4 / n4                                 # frecuencia relativa de C en hold–out
  
  # --- 5 · Construir data.frame con probabilidades históricas, IC y frecuencias recientes
  resultados <- data.frame(
    alpha     = alpha,                                 # niveles de significancia
    P.A       = ci[, "P[A]"],  LI_P.A = ci[, "LI_P[A]"],  LS_P.A = ci[, "LS_P[A]"],  # resultado histórico para A
    P.C       = ci[, "P[C]"],  LI_P.C = ci[, "LI_P[C]"],  LS_P.C = ci[, "LS_P[C]"],  # resultado histórico para C
    FreqAbs_A = kA4,  FreqRel_A = pA4,                 # número y proporción de éxitos A en hold–out
    FreqAbs_C = kC4,  FreqRel_C = pC4                  # número y proporción de éxitos C en hold–out
  )
  
  resultados   # imprime la tabla con todos los resultados: IC históricos + frecuencias de las últimas 4 semanas
  
##=============================== Problema 7            =============================####
## Inciso a y b                                         ####
  
  # Estadísticos muestrales: n, media y varianza
  n  <- length(y)                                         # número de observaciones
  m  <- mean(y)                                           # media muestral
  s2 <- var(y)                                            # varianza muestral
  
  # Cálculo simultáneo de IC para μ y σ² en matriz (filas=α, columnas=mu_inf,mu_sup,sig_inf,sig_sup)
  res <- t(vapply(alpha, \(al) { 
    t_q  <- qt(1 - al/2, n - 1)                           # cuantil t para media
    c_q1 <- qchisq(1 - al/2, n - 1)                       # cuantil χ² superior
    c_q2 <- qchisq(al/2,     n - 1)                       # cuantil χ² inferior
    mu_inf  <- m - t_q * sqrt(s2 / n)                      # límite inferior para μ
    mu_sup  <- m + t_q * sqrt(s2 / n)                      # límite superior para μ
    sig_inf <- (n - 1) * s2 / c_q1                          # límite inferior para σ²
    sig_sup <- (n - 1) * s2 / c_q2                          # límite superior para σ²
    c(mu_inf, mu_sup, sig_inf, sig_sup)                   
  }, numeric(4)))                                         # vector de salida longitud 4
  
  colnames(res) <- c("mu_inf", "mu_sup", "sig2_inf", "sig2_sup")
  resultados    <- data.frame(alpha = alpha, res)         # marco de datos con IC
  
  # Inciso b: extraer IC para gráficas
  muCI <- res[, 1:2]  # columnas mu_inf y mu_sup para cada α
  s2CI <- res[, 3:4]  # columnas sig2_inf y sig2_sup para cada α
  
  dens <- \(µ,σ2,x) dnorm(x, µ, sqrt(σ2))                  # función densidad normal parametrizada
  σmax <- sqrt(max(s2CI))                                 # desviación estándar máxima
  x    <- seq(min(muCI) - 4 * σmax, max(muCI) + 4 * σmax, len = 800)  # rango de x amplio (±4σ)
  
  cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")    # cuatro colores distintos
  ltys <- c(1, 2, 1, 2)                                    # estilos: sólido y punteado
  
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))                 # dos paneles lado a lado
  
  # (b.1) α = 0.005: densidades para μ_inf, μ_sup con σ_inf, σ_sup de fila 1
  matplot(x, cbind(
    dens(muCI[1,1], s2CI[1,1], x),  # densidad (μ_inf, σ_inf)
    dens(muCI[1,1], s2CI[1,2], x),  # densidad (μ_inf, σ_sup)
    dens(muCI[1,2], s2CI[1,1], x),  # densidad (μ_sup, σ_inf)
    dens(muCI[1,2], s2CI[1,2], x)   # densidad (μ_sup, σ_sup)
  ), type = "l", lty = ltys, lwd = 2, col = cols,
  main = "(b.1) μ₁–μ₂ vs σ²₁–σ²₂", xlab = "x", ylab = "f(x)"
  )
  legend("topright",
         c("μ₁,σ²₁ (solid)", "μ₁,σ²₂ (dashed)", "μ₂,σ²₁ (solid)", "μ₂,σ²₂ (dashed)"),
         col = cols, lty = ltys, lwd = 2, bty = "n"
  )
  
  # (b.2) μ₃ = mu_inf[2], μ₂ = mu_sup[1], con las mismas σ_inf y σ_sup de fila 1
  matplot(x, cbind(
    dens(muCI[2,1], s2CI[1,1], x),  # densidad (μ₃, σ_inf)
    dens(muCI[2,1], s2CI[1,2], x),  # densidad (μ₃, σ_sup)
    dens(muCI[1,2], s2CI[1,1], x),  # densidad (μ₂, σ_inf)
    dens(muCI[1,2], s2CI[1,2], x)   # densidad (μ₂, σ_sup)
  ), type = "l", lty = ltys, lwd = 2, col = cols,
  main = "(b.2) μ₃–μ₂ vs σ²₁–σ²₂", xlab = "x", ylab = "f(x)"
  )
  legend("topright",
         c("μ₃,σ²₁ (solid)", "μ₃,σ²₂ (dashed)", "μ₂,σ²₁ (solid)", "μ₂,σ²₂ (dashed)"),
         col = cols, lty = ltys, lwd = 2, bty = "n"
  )
  
  # (b.3) Escenario más pesimista: combinación de μ mínima y σ² máxima
  mu_pes <- min(muCI)                                      # media mínima entre todos los IC
  s2_pes <- max(s2CI)                                      # varianza máxima entre todos los IC
  # define la normal N(mu_pes, s2_pes) como el caso de mayor riesgo
  
##=============================== Problema 8            =============================####
##  Tabla                                               ####

  n1 <- n %/% 2;             n2 <- n - n1        # mitades de la serie
  y1 <- y[1:n1];             y2 <- y[(n1+1):n]   # subconjuntos
  
  m1 <- mean(y1);            m2 <- mean(y2)      # medias
  s1 <- var(y1);             s2 <- var(y2)       # varianzas
  Sp2 <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)       # varianza agrupada
  se  <- sqrt(Sp2*(1/n1 + 1/n2))                 # error estándar de m1−m2
  
  IC  <- t(vapply(alpha, \(a){
    q <- qt(1 - a/2, n1 + n2 - 2);      # cuantil t con g.l. = n1+n2−2
    c((m1-m2) - q*se, (m1-m2) + q*se)   # [LI, LS]
  }, numeric(2)))
  
  data.frame(alpha = alpha,
             LI    = IC[,1],
             LS    = IC[,2])

##=============================== Problema 9            =============================####
##  Tabla                                               ####
  
  p   <- df$close   # Serie de precios de cierre
  v   <- df$volume  # Serie de volúmenes transados
  
  # Para cada nivel de significancia (alpha), usar cor.test para calcular 
  # el intervalo de confianza para la correlación \rho y extraer sus límites
  cis <- t(sapply(alpha, function(a) {
    # cor.test calcula r y construye internamente el IC usando Fisher’s z
    ct <- cor.test(p, v, conf.level = 1 - a)
    # ct$conf.int[1] = límite inferior, ct$conf.int[2] = límite superior
    c(LI = ct$conf.int[1], LS = ct$conf.int[2])
  }))
  
  # Armar el data.frame final que asocia cada alpha con sus límites LI y LS
  data.frame(alpha = alpha, cis)