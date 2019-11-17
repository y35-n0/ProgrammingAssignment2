## Once you get an inverse of a matrix, it caches the inverse.
## Next time you need the inverse, you take the cached inverse and use it.
## When the matrix changes, initialize an inverse of the matrix and cached it.

## Making a matrix that it can take an cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve() <- function(solve) s <<- solve
        getsolve() <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Getting a cached inverse of the matrix.
## If the matrix doesn't take an inverse of the matrix,
## it solves an inverse of matrix and saves it, return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
