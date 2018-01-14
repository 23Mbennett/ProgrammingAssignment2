makeCacheMatrix <- function(x = matrix()) {             ## define special matrix object 
  
  inv <- NULL                                           ## iniciate inv as NULL, which will contain value of the matrix invers 
  
  set <- function (y) {                                 ## set function to assign new 
    
    x <<- y                                               ## value of the matrix in the parent enviroment, <<- 
    
    inv <<- NULL                                          ## if the new matrix occures, reset inv to NULL
    
  }
  
  get <-function () x                                   ## define get fanction,  which returns the value of the matrix 
  
  setinverse <- function(inverse) inv <<- inverse       ## assigns value of inv in parent environment
  
  getinverse <- function() inv                          ## get the value of the inverse fuction
  
  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #assigne the names for each of the functions to use$
  
}





## Write a short comment describing this function



cacheSolve <- function(x, ...) {    
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()                                             ## first we check if inverse has been already calculated
  
  if(!is.null(inv)) {                                              ## if yes, the inv value is returned 
    
    message("getting cached data")
    
    return(inv)
    
  }
  
  data <- x$get()                                              ## if not, we get the new inverse value 
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
  
}