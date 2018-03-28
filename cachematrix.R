## The two functions below, makeCacheMatrix, and cacheSolve use a special
## matrix which can store it's inverse once solved.  This speeds up the use
## of the matrix by preventing it from having to be solved each time.
##
## The functions make use of the R operator <<- which allows the assignment of 
## an opjects value in an environment which is different from the current environment.
## 

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    ## creates a set function in the environment of the new matrix
    ## that defaults the minv to NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL       ##this ensures that if the matrix changes the cache is cleared
    }
    ## creates a get function in the environment of the new matrix
    ## that simply returns the original matrix
    get <- function() x                     
    ## creates a setminv function in the environment of the new matrix
    ## that will set the minv to the value given
    setminv <- function(inverse) minv <<- inverse
    ## creates a getminv function in the environment of the new matrix
    ## that returns the inverted matrix that is cached
    getminv <- function() minv
    list(set = set, get = get,
         setminv = setminv,
         getminv = getminv)
}


## This function returns the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getminv()                 ##get the value in the special matrix cache
    if(!is.null(minv)) {                ##if the inverse is cached then
        message("getting cached data")  ##
        return(minv)                    ## return the cachedinverse
    }
    data <- x$get()         ## uses the fancy matrix get function to return the matrix
    minv <- solve(data, ...)  ## solves the matrix if it wasnt in cache
    x$setminv(minv)         ##puts the answer into the minv variable in the 
                            ##special matrix's environment
    minv                    ##and returns the inverse
}
