# coding: utf-8
import sys
import os
import matplotlib.pyplot as plt
import numpy as np


def graph_results(z,it,title):
	print(" ___________________________________________ ")
	print("graficando :: iteracion ",iteracion)

	x = np.linspace(0,longitud,n+1)
	y = np.linspace(ancho,0,m+1)

	contours = plt.contour(x,y,z,cmap="RdYlGn_r")
	plt.clabel(contours, inline=True, fontsize=8)

	z = list(reversed(z))
	plt.imshow(z, extent=[0, longitud, 0, ancho], origin='lower',cmap='RdYlGn_r', alpha=0.5)
	plt.colorbar()
	plt.ylabel("Y")
	plt.xlabel("X")
	plt.suptitle(title,fontsize=14, fontweight='bold')
	plt.title("iteracion "+str(it))




# check if the file we gived as argument exist
if len(sys.argv) == 2:
	file = sys.argv[1]
	if not os.path.isfile(file):
		print("!!!!!!! The file does not exists !!!!!!!!!!! ")
		sys.exit()
else:
	sys.exit()


# check if a dir call results exists, if not we create one
if not os.path.exists('./img/'):
	os.makedirs('./img/')


# open the file, will be close automatically
with open(file) as f:
	filenamegif = open("./img/filenames.txt","w")
	# z values (temperatures)
	z = []

	#title
	title = input("Introduzca un titulo para las graficas:: ") 

	# read file line by line
	for line in f:
		# jump comments line
		if '#' not in line:

			tokens = line.split();

			if len(tokens) > 0:

				# input data, it is marked with asteriks
				if tokens[0] == "*" and tokens[2] == "=":
					if tokens[1] == "n":
						n = float(tokens[3])
					if tokens[1] == "m":
						m = float(tokens[3])
					if tokens[1] == "Long":
						longitud = float(tokens[3])
					if tokens[1] == "Ancho":
						ancho = float(tokens[3])

				elif tokens[0] == "->":
					# graph the values
					iteracion = int(tokens[-1]) - 1

					if len(z) > 0:
						graph_results(z,iteracion,title)
						savename = "./img/it"+str(iteracion)+".png"
						filenamegif.write("it"+str(iteracion)+".png"+"\n")
						plt.savefig(savename)

						plt.clf()
						z = []
				else:
					# read the matrix of temperatures
					fila = []

					for text in tokens:
						number = float(text)
						fila.append(number)

					z.append(fila)
	if len(z) > 0:
		iteracion = iteracion + 1
		graph_results(z,iteracion,title)
		savename = "./img/it"+str(iteracion)+".png"
		filenamegif.write("it"+str(iteracion)+".png"+"\n")
		plt.savefig(savename)

		plt.clf()
		z = []
		
	filenamegif.close()
	
