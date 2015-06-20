#####################################
## Coursera - R Programming        ##
## Aaron Williams                  ##
## Week 3 - Assignment 2           ##
## Caching the Inverse of a Matrix ##
#####################################


## 2 functions are required to be able to cache the inverse of a matrix

######################################

## This function sets the inverse of the matrix
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Object to store the inverse matrix
  i <- NULL
  
  ## Define the matrix object
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## define a function to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## define a function to invert the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## define a function to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## list the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


######################################
## This functions computes the inverse of the special "matrix" returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Just as it says - gets the inverse of the matrix
  m <- x$getInverse()
  
  ## Check for previously cached result
  if( !is.null(m) ) {
    message("checking cache")
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  m <- solve(data) %*% data
  
  ## Set the inverse
  x$setInverse(m)
  
  ## Show the final result
  m
}

