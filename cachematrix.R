## This pair of functions cache the inversion of a matrix. Matrix inversion
## is a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly. 

## This function creats a special "matrix" object that can cache its inverse. 
## The function is able to: 
## Set the value of the matrix 
## Get the value of the matrix 
## Set the value of the inverse
## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL                           ## Initializes the value of the matrix 
      set <- function(y) {                ## inverse to NULL. Declares another 
              x <<- y                     ## function set where the value will 
              m <<- NULL                  ## be cached. Changes the value of the 
      }                                   ## inverse of the matrix in the event
      get <- function() x                 ## that the matrix was changed. 
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m          ## Gets the value of the inverse and 
      list(set = set, get = get,          ## then calculates the inverse of a non-
           setinverse = setinverse,       ## singular matrix via the solve function. 
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {                      ## Used to get the cache of the matrix
        m <- x$getinverse()
        if(!is.null(m)) {                        ## If the matrix exists, it gets it.
                  message("getting cached data") ## and then exits the program 
                  return(m)                      ## without executing subsequent code. 
        }
        data <- x$get()                          ## If the inverse is not there, 
        m <- solve(data)                         ## it is calculated and then 
        x$setinverse(m)                          ## retrieved. 
        m
}
