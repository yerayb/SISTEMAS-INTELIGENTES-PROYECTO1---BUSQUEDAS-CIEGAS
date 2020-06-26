# =======================================================================
# Group Name:  DerechaHastaBlanco
# Students: Yeray Bellanco Casares
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
library(dplyr)
library(tidyr)
initialize.problem <- function(barra,disco) {
  problem <- list() # Default value is an empty list.
  
  # This attributes are compulsory
  
  #NOMBRE 
  problem$name <- paste0("Torres de Hanoi (", barra, " barras, ", disco, " discos)") 
  
  #ESTADO INICIAL: VECTOR CON TODOS LOS VALORES CON 1
  problem$state_initial     <- rep(1, disco)
  
  #ESTADO FINAL: TODOS LOS VALORES SON IGUALES AL NUMERO DE SOPORTES
  problem$state_final       <-  rep(barra, disco)
  
  #ACCIONES POSIBLE: DEFINIMOS TODAS LAS ACCIONES POSIBLES QUE NOS PERMITEN MOVER DISCOS TANTO EL SOPORTE DE ORIGEN COMO EL SOPORTE DE DESTINO
  problem$actions_possible  <- data.frame(permutations(n=barra,r=2, v=c(1:barra))) #PERMUTACION DEL CONJUNTO DE SOPORTES TOMADOS EN GRUPOS DE 2
  problem$actions_possible <- unite(problem$actions_possible,action,c(1:2), sep=", ", remove = TRUE) #JUNTAMOS EN UNA COLUMNA LLAMADA ACTION
  problem$actions_possible
 
   # ATRIBUTOS ADICIONALES : DISCO Y BARRA DEL PUZZLE DE HANOI
   problem$disco <- disco
   problem$barra <- barra
  
  return(problem)
}

# Analyzes if an action can be applied in the received state.
#PARAMETRO ESTADO: VECTOR CON LA INDICACION DEL SOPORTE EN QUE SE ENCUENTRA CADA DISCO
#PARAMETRO ACCION: NOMBRE DE LA ACCION CON QUE ESPECIFICA EL SOPORTE DE ORIGEN Y DE DESTINO
#OBJETIVO: COMPROBAR SI SE PUEDE REALIZAR EL MOVIMIENTO DESDE EL SOPORTE DE ORIGEN AL SOPORTE DESTINO
is.applicable <- function (state, action, problem) {
  result <- FALSE # Default value is FALSE.
  
  #PASAMOS EL ORIGEN A UNA VARIABLE
  origen <- substr(action, 0, 1)
  origen <- as.numeric(origen)
  
  #PASAMOS EL DESTINO A UNA VARIABLE
  destino <- substr(action, 3, 4) 
  destino <- as.numeric(destino) 
 
  
  #COMPROBAMOS MOVIMIENTO
  if (any(state == origen)) { #COMPROBAR ORIGEN
    if (any(state == origen) && any(state == destino)) { #COMPROBAR ORIGEN Y DESTINO
      if (max(which(state == origen)) > max(which(state == destino))) { #COMPROBAR QUE NUESTRO DISCO ES MAS PEQUEÑO EN EL DESTINO
        result <- TRUE
      }
    } else {
      result <- TRUE
    }
  }
  return(result)
}

#  the state resulting on applying the action over the state
#PARAMETROS: ACCION Y ESTADO
#OBJETIVO: QUE SE EJECUTE CUANDO IS.APPLICABLE() HA DEVUELTO TRUE. CREA UNA COPIA DEL ESTADO Y MODIFICA EL SOPORTE DEL DISCO QUE ESTA MAS ARRIBA
# EN EL SOPORTE ORIGEN PARA COLOCARLO  EN EL SOPORTE DESTINO

effect <- function (state, action) {
  #PASAMOS EL ORIGEN A UNA VARIABLE
  origen <- substr(action, 0, 1)
  origen <- as.numeric(origen)
  
  #PASAMOS EL DESTINO A UNA VARIABLE
  destino <- substr(action, 3, 4) 
  destino <- as.numeric(destino) 
  
  state[max(which(state==origen))] <- destino

  result <- state # Default value is the current state.
  
  return(result)
}

# Analyzes if a state is final or not
#OBJETIVO: INVOCAR A LA FUNCION ALL()
is.final.state <- function (state, final_state) {
  result <- FALSE # Default value is FALSE.
  #FUNCION ALL()
  result <- (all(state==final_state))
  return(result)
}

# Transforms a state into a string
to.string = function (state) {
  print(state) 
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE COST OF APPLYING THE ACTION ON THE STATE> 
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
  # <INSERT YOUR CODE HERE TO RETURN THE RESULT OF THE EVALUATION FUNCTION>
  
  return(1)
}

#-------- PRUEBAS UNITARIAS -------------
library(testthat)

#CREAMOS VARIOS ESTADOS DE PRUEBA
estado_1 <- data.frame( states = c(rep(1,3))) 
estado_2 <- data.frame( states = c(rep(4,3)))
estado_3 <- data.frame( states = c(rep(2,3)))
estadoFinal <- data.frame( states = c(rep(4,3))) 
problem <- initialize.problem(6,3) 

#///// VERIFICAR MOVIMIENTOS - SE PUEDE O NO SE PUEDE ///////
test_that("NO SE PUEDE",{
  expect_true(is.final.state(estado_1, estadoFinal))
})


test_that("SE PUEDE",{
  expect_true(is.final.state(estado_2, estadoFinal))
})

#///// VERIFICAR ESTADO FINAL - ES O NO ES ///////

test_that("NO es estado final",{
  expect_true(is.final.state(estado_1, estadoFinal))
})

test_that("SI es estado final",{
  expect_true(is.final.state(estado_2, estadoFinal))
})

#///// VERIFICAR MOVIMIENTOS - SE HACEN O NO SE HACEN  ///////
#MOVER(2,4)
test_that("SE HACE EL MOVIMIENTO",{
  estado_1 <- effect(estado_1,2)  
})

#MOVER(6,3)
test_that("NO SE HACE EL MOVIMIENTO",{
  estado_3 <- effect(estado_3,28) 
})

