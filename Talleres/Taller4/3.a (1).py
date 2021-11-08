import numpy as np
import matplotlib.pyplot as plt

def f(t, xyz):
    (x, y, z) = xyz
    a = 8/3
    b = 10
    c= 28
    return (np.array([(a*x)+(y*z), b*(y-z), (((-1)*x)*y)+(c*y)-z]))


def Euler(x0, n, h, t):
    h = 1/100000
    sol = np.zeros((3, n+1))
    sol[:, 0] = x0
    for i in range (1, n+1):
        sol[:, i] = (sol[:, i-1] + h*f(t[i-1], sol[:, i-1]))
    return sol

t = np.round(np.linspace(0, 100, 10001), 4)
h = 0.5
xyz = [1, 1, 1]
x = Euler(xyz, 10000, h, t)

print("Valor de x cuando t=100: ", round(x[0, 10000], 4))
print("Valor de y cuando t=100: ", round(x[1, 10000], 4))
print("Valor de z cuando t=100: ", round(x[2, 10000], 4))

plt.plot(t, x[0], label = "x")
plt.plot(t, x[1], label = "y")
plt.plot(t, x[2], label = "z")
plt.xlabel("t")
plt.ylabel("x, y, z")
plt.legend()
plt.show()


for i in range(len(x[1])):
    if round(x[1, i], 4) == 0:
        print("Punto de equilibrio para y en t=", t[i])

