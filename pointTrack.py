# coding: utf-8
import sys
import os
import matplotlib.pyplot as plt
import numpy as np





resultsfilename = "resultados.txt"
filesavename = sys.argv[1]
plotsavename = sys.argv[2]




def read_points():
	print("Escribe los puntos a seguir, separa por una coma sus indices, ejemplo 2,1.")
	print("Cuando quiera parar, escriba -1.")
	i = 1
	user_points = []
	user_text = ""
	while not user_text == "-1":
		user_text = input("punto "+str(i)+": ")
		if not user_text == "-1":
			text_points = user_text.split(",")
			points = []
			for text in text_points:
				point = int(text)
				points.append(point)
			user_points.append(points)
			print("["+str(points[0])+","+str(points[1])+"]")
		else:
			print("no more points -> just run !!!")
		i += 1

	return user_points


with open(resultsfilename) as f:
	puntos = read_points()

	w = open(filesavename,"w")
	for punto in puntos:
		w.write("Punto [{:3d},{:3d}]  ".format(punto[0],punto[1]))

	w.write("\n------------------------------------------------------------------------------------------------\n")

	# z values (temperatures)
	z = []

	# temperature in points
	pointTemp = [[] for i in range(len(puntos))]

	# read file line by line
	for line in f:
		# jump comments line
		if '#' not in line:

			tokens = line.split();

			if len(tokens) > 0:
				if tokens[0] == "*":
					continue
				elif tokens[0] == "->":
					# graph the values
					iteracion = int(tokens[-1]) - 1

					if len(z) > 0:
						try:
							for i in range(len(puntos)):
								punto = puntos[i]
								pointTemp[i].append(z[punto[0]][punto[1]])
							for punto in puntos:
								w.write('{:<15f}  '.format(z[punto[0]][punto[1]]))
							w.write("\n")
						except(IndexError):
							print("CHAVAL NO ME CHINGUES COGIENDO PUNTOS FUERA DE LA PLACA")
							sys.exit(1)
						z = []
				else:
					# read the matrix of temperatures
					fila = []

					for text in tokens:
						number = float(text)
						fila.append(number)

					z.append(fila)
	if len(z) > 0:
		# last iteration 
		for punto in puntos:
			w.write('{:<15f}  '.format(z[punto[0]][punto[1]]))
		z = []
		
	w.close()

	# printing and saving fig
	for i in range(len(puntos)):
		punto = puntos[i]
		name = "Punto [{:d},{:d}]  ".format(punto[0],punto[1])
		plt.plot(pointTemp[i], label=name)

	legend = plt.legend(bbox_to_anchor=(0.97,1.0))
	plt.ylabel("Temperatura [Kelvin]")
	plt.xlabel("Step")
	titulo = input("\n Titulo del grafico :: ")
	plt.title(titulo,fontsize=14, fontweight='bold')
	plt.savefig(plotsavename, bbox_extra_artists=(legend,), bbox_inches='tight')
	print("Se a creado un grafica con los datos en el archivo -> "+plotsavename)