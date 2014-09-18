## Caching the inverse of matrix ( Created using caching mean of vector example )

## This function creates a special object that stores a Matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #set the value of matrx
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      # get the value of matrix 
      get <- function() x
      
      # set the inverse of matrix 
      setsolve <- function(solve) m <<- solve
      
      # get the inverse of matrix 
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

##The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if the inverse 
##has already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data 
##and sets the value of the Inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## get the inverse of the matrix
      m <- x$getsolve()
      # check if there is already inverse 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if not: get the inverse of the matrix  
      data <- x$get()
      m <- solve(data, ...)
      ## set the inverse of the matrix 
      x$setsolve(m)
      m
}
