##############################################################################
#                          Universidad de San Andrés                         #
#            Métodos Econométricos y Organización Industrial Aplicada        #
#                             Trabajo Practico N° 2                          #
#                                   Julio 2025                               #
#      Luca Bentivenga, Nina Di Costanzo Pereira y María Luján Puchot        #
##############################################################################


import pandas as pd
import numpy as np
import os

# ---- Definimos el directorio base y las subcarpetas ----
main = '/Users/ninadicostanzopereira/Desktop/Métodos/metodos_econometricos/TP2'  # ajuste esto si necesario
input_dir = os.path.join(main, 'input')
output_dir = os.path.join(main, 'output')

# ---- Cargamos le file ----
input_file = os.path.join(input_dir, 'TP_2_data.xlsx')
df = pd.read_excel(input_file)

df.head

# --- busco mercados unicos ----
df['market_id'] = df['tienda'].astype(str) + '_' + df['semana'].astype(str)
print(df['market_id'].nunique())

# consumidores

# valores iniciales
s_0 = 0.36
df['market_share'] = df['ventas'] / df['cantidad']
df['delta_ini'] = np.log(df['market_share']) - np.log(s_0)

def simular_consumidores_por_mercado(df, N=50, seed=42):
    np.random.seed(seed)
    mercados = df['market_id'].unique()
    mercados = "3" #POR FAVOR BORRÁ ESTO
    consumidores = {}
    for m in mercados:
        v_i = np.random.normal(size=N)
        I_i = np.random.normal(size=N) #log normal mirar el promedio de ingresos por tienda
        consumidores[m] = {'v_i': v_i, 'I_i': I_i}
    return consumidores

consumidores = simular_consumidores_por_mercado(df, N=2)

print(consumidores)






