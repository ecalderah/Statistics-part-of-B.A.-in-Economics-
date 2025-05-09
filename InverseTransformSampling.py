# Tarea 17
# Simulación de transformación inversa para muestrear una variable aleatoria X cuya función de distribución es F(x)=1/1-a^{1/3)

# librerias
    import numpy as np
    import matplotlib.pyplot as plt

# Convertimos los valores de alpha al quantil de forma 1/1-a^{1/3)
    def cuantil_X(alpha):
        if alpha <= 0:
            return 1.0
        elif alpha >= 1:
            return float('inf')
        else:
            return 1.0 / (1.0 - alpha) ** (1.0 / 3.0)

    # función para la CDF Original
    def F(x):
        return 1 - 1.0 / x ** 3

    #Realiza la simulación usando el método de transformación inversa y luego grafica el histograma de F(X).
    def simulationfixed():
        N = 1000000
        u = np.random.rand(N)
        X_samples = [cuantil_X(a) for a in u]

        # Aplicamos la F(x) original a cada muestra X para recuperar la variable uniforme.
        v_recovered = [F(x) for x in X_samples]

        # Graficamos el histograma de vau_recuperado.
        plt.figure(figsize=(8, 4))
        plt.hist(v_recovered, bins=50, density=True, alpha=0.7, color='steelblue')
        plt.title('Histograma de F(X)')
        plt.xlabel('F(X)')
        plt.ylabel('Densidad estimada')
        plt.grid(True)
        plt.xlim(0, 1)  # Aseguramos que el eje x se encuentre en [0,1]
        plt.show()

# Ejecución final.
simulationfixed()
