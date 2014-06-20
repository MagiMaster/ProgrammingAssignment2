## Functions to create and use a cached matrix inverse

## Create a cached matrix solution object using the given matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Set the solution to null to begin with
  s <- NULL
  
  ## create the set function which sets the matrix and resets the solution
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## create the get function which just returns the stored matrix
  get <- function() x
  
  ## create the setsolve function which stores the solution
  setsolve <- function(sol) s <<- sol
  
  ## create the getsolve function which returns the stored solution
  getsolve <- function() s
  
  ## return the list of functions
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Solve a cached matrix and store the solution
## Note that the ... arguments are ignored if a cached solution exists
cacheSolve <- function(x, ...) {
  ## Get the cached solution
  s <- x$getsolve()
  
  if(!is.null(s)) {
    message("Getting cached solution")
  } else {
    ## If the cached solution doesn't exist, calculate it and cache the results
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
  }

  ## Return the solution
  s
}
