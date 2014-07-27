## These two functions work together taking advantage of R's 
## lexical scoping rules to access environment variables x
## and inv_m.  The purpose of the functions is to allow cacheing
## of the inverse of a matrix so that it does not have to be
## recalculated each time the inverse is desired.


## This function creates the list which includes the matrix
## and some functions (set, get, setInverse, getInverse)

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv_m <<- solve
        getInverse <- function() inv_m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## this function returns the inverse from the cache (inv_m) if
## it exists.  If it doesn't exist it calculates it, stores
## it in the cache (inv_m) and returns the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setInverse(inv_m)
        inv_m
}
