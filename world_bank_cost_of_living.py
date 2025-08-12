import pandas as pd # Manipular tablas tipo excel
import requests # Acceder a la API

# Descargamos el IPC en %, directamente de la API del Banco Mundial)
INPC = 'FP.CPI.TOTL.ZG'  # Linea exacta para pedir el INPC
countries = ['BOL','BRA','CHL','COL','MEX'] # países
start_year, end_year = 1995, 2024 # Tiempo

# Creamos una variable que contenga un URL (obligatorio por la API) para pedir el la database
url = (f"http://api.worldbank.org/v2/country/{';'.join(countries)}/indicator/{INPC}" # Generamos la variable URL para usarla depues
       f"?date={start_year}:{end_year}&format=json&per_page=2000")  # la estrucutra del URL es así, nomas se rellena con nuestra info.

inflation = (pd.json_normalize(requests.get(url, timeout=10).json()[1])  # Ponemos un timeout de 10s y descargamos unicamente las observaciones ([1] = datos) del JSON
       .loc[:, ['country.value','date','value']] # Seleccionamos las columnas que ocupamos
       .rename(columns={'country.value':'country','date':'year','value':'inf'})  # renombramos las columnas (%)
       .dropna(subset=['inf']) # Elimina filas sin datos
       .astype({'year': int, 'inf': float}) # Asignamos a los años cómo numeros enteros y los datos con coma flotante para evitar problemas.
       .pivot(index='year', columns='country', values='inf')  # Pasamos de formato ancho a Largo (pivot) para una tabla mas amigable
       .sort_index()) # Ordena de las fechas de menor a mayor

print(inflation)

import matplotlib.pyplot as plt # Necesario para graficar

ax = inflation.plot(figsize=(12,7), marker='o', grid=True, title='Inflación anual (%) — 1995–2024') # Tamaño, puntos y título.
ax.set(xlabel='Año', ylabel='Inflación (%)') # Etiquetas del eje X y Y
ax.legend(title='País') # legenda
plt.xticks(inflation.index[::5]) # Cada cuantos años de marca
plt.tight_layout() # Marco para evitar problemas con el texto
plt.show()

# Calcular el factor acumulado para cada país
factor_acumulado = (1 + inflation / 100).prod() #Aplicamos la ecuación transformando de % a decimales para poder hacer la operación
factor_acumulado = factor_acumulado.sort_values(ascending=False) # Organizamos de mayor a menor por gusto

print(factor_acumulado)

# Cual país tubvo el mayor incremento acumuado en el costo ded la canasta básica según factor.
more_increased = factor_acumulado.idxmax()
more_value = factor_acumulado.max()

print("País con mayor incremento acumulado:", more_increased, "con un valor acumulado de:", more_value)

# Graficas comparativas
factor_acumulado.plot(kind='barh', figsize=(8,5), color='Grey')
plt.xlabel("Factor acumulado")
plt.title("Incremento total acumulado del costo de vida (1995–2024)")
plt.grid(axis='x', linestyle='--', alpha=0.7)
plt.show()