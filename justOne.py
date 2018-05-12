import sys
import os

filename ="./img/filenames.txt"
tempfilename = sys.argv[1]


with open(filename) as f:
	tempfile = open(tempfilename,'w')
	i = 1
	step = int(input("Cada cuanto quiere las imagenes?: "))
	for line in f:
		if(i%step==0 or i==1):
			tempfile.write(line)
		i += 1
	if not line == "":
		tempfile.write(line)
	tempfile.close()



	