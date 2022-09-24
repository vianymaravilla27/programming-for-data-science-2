"Práctica No.1

 Autoras: 
  -Elizalde Baez Regina
  -Maravilla Pérez Vianey
  
  Fecha: 31 DE Agosto de 2021
  
Descripción del problema: Realizar un script en R que al leer
la masa y la estatura de una persona permita calcular su IMC e indique 
de acuerdo a su IMC lo siguiente.

 Fórmula IMC= MASA/ALTURA "

 
peso <- readline("Ingrese peso kg\n")
peso <- as.double(peso)
altura <- readline("Ingrese peso altura\n")
altura <- as.double(altura)
imc <- peso / altura
cat ("Resultado, su IMC es: ", imc)

if(imc >= 40.0){
  cat("\nObesidad de clase III\n")
  
}else{
  if(imc>35.0 && imc<=39.9){
    cat("\nObesidad de clase II\n")
  
}else{
  if(imc>30.0 && imc<=34.9){
    cat("\nObesidad de clase I")

    }
  }
}

if(imc>25.0 && imc<=29.9){
  cat("\nPreobesidad")
}else{
  if(imc>=25.0 && imc<25.9){
    cat("\nSobrepeso")
  }else{
    if(imc>=18.5 && imc<24.9){
      cat("\nIntervalo normal")

    }else{
      if(imc<18.5){
        cat("\nInsuficiencia ponderal")
      }
    }
  }
}

