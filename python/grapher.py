# coding: utf-8
import matplotlib.pyplot as plt
import numpy as np





with open('probando.txt') as f:
	ismatrix = 0
	z = []
	for line in f:
		if '#' not in line:
			tokens = line.split();
			#print(tokens)
			if len(tokens) > 0:
				if tokens[0] != "->" and ismatrix==0:
					n, m, longitud, ancho = tokens
					n = float(n)
					m = float(m)
					longitud = float(longitud)
					ancho = float(ancho)
					x = np.linspace(0,longitud,n+1)
					y = np.linspace(ancho,0,m+1)
					print(n,m,longitud,ancho)
					print(x,y)
					ismatrix = 1
				elif tokens[0] == "->":
					print("iteracion ",tokens[-1])
					if len(z) > 0:
						contours = plt.contour(x,y,z,cmap="RdYlGn_r")
						plt.clabel(contours, inline=True, fontsize=8)
						z = list(reversed(z))
						plt.imshow(z, extent=[0, n, 0, m], origin='lower',cmap='RdYlGn_r', alpha=0.5)
						plt.colorbar()
						plt.savefig("prueba"+tokens[-1]+".png")
						plt.clf()
						z = []
				else:
					fila = []
					for text in tokens:
						number = float(text)
						fila.append(number)
					z.append(fila)
					print(z)
	