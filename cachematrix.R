## We are going to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 ## init the inverse
                m <- NULL
                #set inverse
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                #get inverse
                get <- function() x
               
                setinv <- function(inv) m <<- inv
    
                getinv <- function() m
                ## store the methods in a list so that the can be accessed using$
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        ## Return a matrix that is the inverse of 'x'
        m <- x$inv()
##if not null get cached,   if it is null then calculate it and assign it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
