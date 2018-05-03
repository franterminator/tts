
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
					ismatrix = 1
				elif tokens[0] == "->":
					print("iteracion ",tokens[-1])
				else:
					z.append(tokens)
	print(z)