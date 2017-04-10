
## Below you find functions that cache ("remember" data in short term memory) the inverse of a matrix. 
## Cache is often useful because it makes computations cost effective and returns results faster than without cashe. 
## The makeCacheMatrix function, creates a list, that "saves" the solve of the matrix, after
## the first call of cacheSolve.
## If cacheSolve is called again, before another run of makeCacheMatrix, the result will be grabbed from the list.

## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(
                set = set
                ,get = get
                ,setInverse = setInverse
                ,getInverse = getInverse
        )
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}


