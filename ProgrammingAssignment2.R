## ProgrammingAssignment2
## makeCacheMatrix and cacheSolve

## With makeCacheMatrix function we will create a specific "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m<<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## With cacheSolve function we will estimate the inverse of the specific "matrix" returned by the aforementioned makeCacheMatrix function.
## Note that in case the inverse has already been estimated and the matrix is the same, the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
