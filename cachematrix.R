
## CACHEMATRIX contains two functions, MAKECACHEMATRIX and 
## CACHESOLVE that create a special matrix and stores it in
## cache then computes the matrix inverse, also stores it in
## chache and returns the inverse to the user

makeCacheMatrix <- function(x = matrix()){    ## make Cache function
  
    ##  accepts values for a matrix and saves the inverse of the matrix
    ##  so that it can be retrieved from memory later rather than being
    ##  calculated each time the function cacheSolve is called
  
    m <- NULL
    set <- function(y) {    ## set function
      x <<- y
      m <<- NULL
    }     ##set function
    get <- function() x
    ##  get defined as the matrix 'x' 
    savematrix <- function(z) m <<- z
     getmatrix <- function() m
    list(set = set, get = get,
       savematrix = savematrix,
       getmatrix = getmatrix) 
}    ## make Cache function


cacheSolve <- function(x, ...){    ##cache function
  
  ## Returns a matrix that is the inverse of x
  
    m <- x$getmatrix()
    if(!is.null(m)) {    ##if1
        message("getting cached data")
        return(m)
    }    ##if1
    data <- x$get()
    m <- solve(data, ...)
    x$savematrix(m)
    m  
}   ##cache function