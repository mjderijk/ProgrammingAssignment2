## These two functions facilitate caching of potentially time-consuming computations
## associated with calculating the inverse of a matrix.
## The functions take advantage of the scoping rules of the R language and how
## they can be manipulated to preserve state inside of an R object.

## This function creates an object which has four functions and two objects:
## set(y)               # stores a matrix in 'x'
## get()                # returns 'x'
## setinverse(solve)    # stores an inversed matrix in 'inv'
## getinverse()         # returns an inversed matrix, 'inv'
## 'x'                  # a matrix object; has scope in makeCacheMatrix
## 'inv'                # an object for an inverse matrix; has scope in makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'.
## 
## If 'x' has an inversed matrix stored (in the cache), that matrix is returned via
## 'return(inv)'. Otherwise, the current matrix is retrieved, its inverse is computed
## via 'solve', the result is stored in 'x' and the value is returned via 'inv'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
