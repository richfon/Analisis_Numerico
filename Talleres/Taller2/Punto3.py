
# Método de Gauss
import numpy as np
from math import e

# INGRESO
A = np.array([[10, 100, e**1.5],
              [15, 225, e**2.25],
              [20, 400, e**3]])

B = np.array([[25],
              [130],
              [650]])

# PROCEDIMIENTO
casicero = 1e-50  # Considerar como 0

# Evitar truncamiento en operaciones
A = np.array(A, dtype=np.float32)

# Matriz aumentada
AB = np.concatenate((A, B), axis=1)
AB0 = np.copy(AB)

# Pivoteo parcial por filas
tamano = np.shape(AB)
n = tamano[0]
m = tamano[1]

# Para cada fila en AB
for i in range(0, n - 1, 1):
    # columna desde diagonal i en adelante
    columna = abs(AB[i:, i])
    dondemax = np.argmax(columna)

    # dondemax no está en diagonal
    if dondemax != 0:
        # intercambia filas
        temporal = np.copy(AB[i, :])
        AB[i, :] = AB[dondemax + i, :]
        AB[dondemax + i, :] = temporal
AB1 = np.copy(AB)

# eliminación hacia adelante
for i in range(0, n - 1, 1):
    pivote = np.float32(AB[i, i])
    adelante = i + 1
    for k in range(adelante, n, 1):
        factor = np.float32(AB[k, i] / pivote)
        AB[k, :] = np.float32(AB[k, :] - AB[i, :] * factor)

# sustitución hacia atrás
ultfila = n - 1
ultcolumna = m - 1
X = np.zeros(n, dtype=np.float32)

for i in range(ultfila, 0 - 1, -1):
    suma = 0
    for j in range(i + 1, ultcolumna, 1):
        suma = np.float32(suma + AB[i, j] * X[j])
    b = np.float32(AB[i, ultcolumna])
    X[i] = np.float32((b - suma) / AB[i, i])

X = np.transpose([X])
"""
SALIDA
print('Matriz aumentada:')
print(AB0)
print('Pivoteo parcial por filas')
print(AB1)
print('eliminación hacia adelante')
print(AB)
"""
print('\nSolución: ')
np.set_printoptions(precision=4)
print(X)
print('\n')

k1 = X[0]
k2 = X[1]
k3 = X[2]


def bisect(f, a, b, TOL):
    mid = np.longdouble(a)
    iterations = 0
    while (b-a) >= TOL:
        mid = np.longdouble((a+b)/2)
        result = np.longdouble(f(mid))
        if result == 0:
            break
        if result*f(a) < 0:
            b = np.longdouble(mid)
        else:
            a = np.longdouble(mid)
        iterations += 1
    print(f'{mid:.4f}')


def f(x):
    return k1*x+k2*x**2+k3*e**(0.15*x)-1500


def f1(x):
    return k1*x+k2*x**2+k3*e**(0.15*x)-1800


def f2(x):
    return k1*x+k2*x**2+k3*e**(0.15*x)-2000


print('Dias que tarda llegar a 1500 infectados: ')
bisect(f, 15, 30, 1.0e-56)
print('Dias que tarda llegar a 1800 infectados: ')
bisect(f1, 15, 30, 1.0e-56)
print('Dias que tarda llegar a 2000 infectados: ')
bisect(f2, 15, 30, 1.0e-8)
