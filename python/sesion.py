# coding: utf-8
import matplotlib.pyplot as plt
import numpy as np
def f(x, y):
    return np.sin(x) ** 10 + np.cos(10 + y * x) * np.cos(x)
x = np.linspace(0,5,50)
y = np.linspace(0,5,40)
print(x)
xg, yg = np.meshgrid(x,y)
print(xg)
z = f(xg,yg)
print(z)
contours = plt.contour(x,y,z,4,colors="black")
plt.clabel(contours, inline=True, fontsize=8)

plt.imshow(z, extent=[0, 5, 0, 5], origin='lower',
           cmap='RdGy', alpha=0.5)

plt.colorbar()

plt.savefig("prueba.png")
