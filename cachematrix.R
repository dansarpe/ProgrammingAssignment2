## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  
  ## Inicializa la propiedad inversa
  inv <- NULL
  
  ## Método para configurar la matriz
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Método para obtener el valor de la matriz
  get <- function(){x}
  
  ## Método para establecer la inversa de la matriz
  setInverse <- function(inverse){ inv <<-  inverse}
  
  ## Método para obtener la inversa de la matriz
  getInverse <- function(){
    ## Devuelve la propiedad inversa
    inv
  }
  
  ## Devuelve una lista de los métodos
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null (inv)){
    message("Obteniendo datos del cache")
    return(inv)
  }
  
  ## Obtener la matriz de nuestro objeto
  mat <- x$get()
  
  ## Calcula la inversa usando la multiplicación de matrices
  inv <- solve(mat, ...)
  
  ## Establecer la inversa al objeto
  x$setInverse(inv)
  
  ## Devuelve la matriz
  inv
}
