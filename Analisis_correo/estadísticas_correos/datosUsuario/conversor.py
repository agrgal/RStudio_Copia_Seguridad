# *-* coding: utf-8 *-*

from datetime import datetime

def escritura (l):
	if l:
		l = l.rstrip("\n")
		l = l.replace("T"," ")
		l = l.replace(".000Z","")
		l = l.replace("Nunca","-")	
		
		# Cálculo de fechas
		# =================
		valores=l.split(",")
		email = valores[0]		
		# fechaIMAP = valores[1]
		# fechaPOP = valores[2]
		# fechaWeb=valores[3]
		# datetime.strptime(inDate, "%d-%b-%Y-%H:%M:%S")
		if valores[1]!="-":
			fechaIMAP = datetime.strptime(valores[1],"%Y-%m-%d %H:%M:%S")
		else:
			fechaIMAP=datetime.strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
			
		if valores[2]!="-":
			fechaPOP = datetime.strptime(valores[2],"%Y-%m-%d %H:%M:%S")
		else:
			fechaPOP=datetime.strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
			
		if valores[3]!="-":
			fechaWeb = datetime.strptime(valores[3],"%Y-%m-%d %H:%M:%S")
		else:
			fechaWeb=datetime.strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
		
		if valores[4]!="-":
			fechaDrive = datetime.strptime(valores[4],"%Y-%m-%d %H:%M:%S")
		else:
			fechaDrive=datetime.strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
			
		if valores[5]!="-":
			fechaClassroom = datetime.strptime(valores[5],"%Y-%m-%d %H:%M:%S")
		else:
			fechaClassroom=datetime.strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")
			
			
		print("Fechas %s , %s, %s" % (fechaPOP,fechaIMAP, fechaWeb))
		
		fechaMaxCorreo=max(fechaPOP,fechaIMAP,fechaWeb)
		fechaMaxGlobal=max(fechaMaxCorreo,fechaDrive,fechaClassroom)
		
		if encontrar(l,email):
			archivo2.write(encontrar(l,email)+","+str(fechaMaxCorreo)+","+str(fechaMaxGlobal)+"\n") # no permite escribir la cabecera
	
def encontrar(ll, dato):
	archivo3.seek(0)
	rr=""
	l2="***"
	while l2 !="":
		l2=archivo3.readline()
		if dato in l2:
			explotar=l2.split(",")
			rr = explotar[0]+","+explotar[1]+","+explotar[22]+","+ll #22 corresponde a departamento
			break
	return rr
	
	

#1) Abrir fichero
archivo1 = open("fechas.csv")
archivo3 = open("alumnado.csv")
archivo2 = open("convertido.csv", "w")
# lineas1 = archivo1.readlines()

archivo2.seek(0) # apunta a la primera línea
linea1 = archivo1.readline() # ¡Ojo!, readline no readlineS
archivo2.write("Nombre,Apellidos,Clase,Email,IMAP,POP,Correo Web, Drive, Classroom, correoMax,Global\n")
while linea1 !="":
    linea1 = archivo1.readline() # ¡Ojo!, readline no readlineS
    escritura(linea1)     
    # print (linea1)

archivo1.close()
archivo2.close()
archivo3.close()

