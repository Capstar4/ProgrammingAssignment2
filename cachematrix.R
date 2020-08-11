## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL                 ## initialise inverse as null
  ## set matrix
  set <- function(y){
    x <<- y
    a<<- NULL
  }
  get <- function() x                          ## get matrix
  setInverse <- function(inverse) a <<- inverse    ## set inverse of matrix
  getInverse <- function() a           ## get inverse of matrix
  ## return list of methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  a<- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)                        ##calculates inverse
  x$setInverse(a)
  ## return the matrix that is inverse of x
  a
}

