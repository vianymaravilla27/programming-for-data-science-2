
alt_var <- c(1.81,1.75,1.76,1.75,1.74)
alt_fem <- c(1.65, 1.72, 1.54,1.27,1.57)

cont_var <- 0
cont_fem <- 0

for(i in alt_var){
    if(i >= 1.75){
      cont_var <- cont_var+1
    }
  
}

for(i in alt_fem){
  if(i >= 1.65){
    cont_fem <- cont_fem + 1
  }
  
}

cat("la cantidad de integrantes del equipo varonil es",cont_var,"\n la cantidad de integrantes del equipo
    femenil es,",cont_fem)