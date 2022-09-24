"Practica No.1

 Autoras: 
  -Elizalde Baez Regina
  -Maravilla Perez Vianey
  -Fecha: 31 DE Agosto de 2021
  
Descripcion del programa:
calcular  Realiza un script que permita obtener la hipotenusa de un triángulo
rectangulo,el programa deberá preguntar si desea obtener otra hipotenusa, si es 
así repetir"


hipotenusa <- function(){
  cateto_Opuesto <- readline(prompt = "Introduce el cateto opuesto del triangulo:") 
  cateto_Adyascente <- readline(prompt = "Introducel cateto adyacente del triangulo:")
  
  cateto_Opuesto<- as.numeric(cateto_Opuesto) 
  cateto_Adyascente<- as.numeric(cateto_Adyascente) 
  
  hip <- sqrt(cateto_Adyascente ** 2 + cateto_Opuesto ** 2) 
  
  print(paste("El resultado de la hipotenusa del triangulo rectangulo es: ", hip)) 
}


ciclo <- function(){
  pregunta <- readline(prompt = "¿Quisieras de nuevo calcular la hipotenusa de un triagulo ? 1=Si, quiero calcular 2=No quiero calcular ") 
  while(pregunta == 1){ 
    hipotenusa() 
    pregunta <- readline(prompt = "¿Quieres de nuevo calcular la hipotenusa de un triagulo? 1=Si, quiero calcular 2=No quiero calcular ")  
  }
}





 
 
