#!/usr/bin/python3
# First argument is path to data files
# Second argument is time in seconds between checks of new files
# (Optional) if third argument is 1, then start by running last REMESAS file

import sys, getopt
import os
import time
import subprocess

min_seleccionados = ['00','15','30','45']
def procesar_nombre(filename):
  descriptors = {'tipo':'Desconocido'}
  try:
    nombre, ext = filename.split(".")
    if(ext=='txt'):
      codigo = nombre[-10:]
      tipo = nombre[:-10]
      eleccion_tipo = codigo[:2]
      id_estado = codigo[3:5]
      fecha = codigo[4:10]
      descriptors = {'nombre':nombre, 'tipo':tipo, 
          'eleccion_tipo':eleccion_tipo, 
          'id_estado':id_estado,
          'fecha':fecha}
  except Exception as ex:
    print("Error procesando archivo")

  return descriptors

def main(argv):
  data_path = argv[0]
  path_out = argv[1]
  path_results = argv[2]
  wait_sec = argv[3]
  last = 0
  team = "default"
  if(len(argv) > 4):
    team = argv[4]
  if(len(argv) == 6):
    last = 1  
  if not os.path.exists(path_out):
    os.makedirs(path_out)
  print("Observando " + data_path + " cada "+ wait_sec + " segundos.")
  files_before = [f for f in os.listdir(data_path) if f[:7] == "REMESAS"]
  files_before.sort()
  if(last==1):
    files_before = files_before[:-1]
  while 1:
    files_now = [f for f in os.listdir(data_path) if f[:7] == "REMESAS"]
    added = [f for f in files_now if not f in files_before]
    if added:
      print(".")
      added.sort()
      print("Se agregaron: ", ",".join(added))
      # use last one
      # added = added[-1:]
      for filename in added:
        descriptores = procesar_nombre(filename)
        print(descriptores)
        if(descriptores["tipo"] == "REMESAS" and (descriptores["fecha"][-2,0] in min_seleccionados)):
          full_path = data_path + "/" + filename
          subprocess.call(["r", "-e", "quickcountmx:::process_batch_stan('" +full_path+"','"+descriptores['nombre']+"','"+path_out+"','"+path_results+"','"+team+"')"])
    else:
      print('.', end = '', flush = True)
    files_before = files_now
    time.sleep(int(wait_sec))
  
if __name__ == "__main__":
   main(sys.argv[1:])
