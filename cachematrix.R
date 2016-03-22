## Fco. Javier Montesinos
## 22 de marzo de 2016
## Programming Assignment 2: Lexical Scoping

## Esta funcion almacena una matriz y su inversa

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Esta función obtiene la inversa de una matriz generada con makeCacheMatrix
# Evalua si el argumento pasado como primer parámetro es una matrix cuadrada 
# no singular para poder calcular su inversa.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # El argumento x no es una matriz
  if(!is.matrix(x$get())) {
    stop("El argumento no es una matriz")
  }
  
  # El argumento x no es una matriz cuadrada
  if(ncol(x$get()) != nrow(x$get())){
    stop ("El argumento no es una matriz cuadrada")
  }
  
  # Si el determinante de la matriz es 0 -> es singular -> no podremos
  # obtener la matriz inversa -> reportamos el error controlado
  if(det(x$get()) == 0){
    stop("El argumento x es una matriz singular")
  }
  
  s <- x$getsolve()
  
  # Si el objeto pasado como parámetro ya ha calculado la matriz inverse ->
  # se retorna el valor en cache, i.e. no se calcula la inversa de la matriz.
  if(!is.null(s)) {
    message("Obteniendo valor de cache del objeto")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
