## A pair of functions that cache the inverse of a matrix
## R Programming Week 3 Programming Assignment 2: Lexical Scoping

## Input matrix, cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        matrix.inverse <- NULL
        clear <- function(y){
                x <<- y
                matrix.inverse <<- NULL
        }
        matrix.collect <- function() x
        inverse.set <- function(solve) matrix.inverse <<- solve 
        inverse.get <- function() matrix.inverse
        list(matrix.inverse=matrix.inverse,matrix.collect=matrix.collect,
             inverse.set=inverse.set,inverse.get=inverse.get)
        
}


## check cache, inverse matrix

cacheSolve <- function(x,...) {
        matrix.inverse <- x$inverse.get()
        if (!is.null(matrix.inverse)) {
                message("Collecting Cached Data")
                return(matrix.inverse)
                
        }
        data <- x$matrix.collect()
        matrix.inverse <- solve(data,...)
        x$inverse.set(matrix.inverse)
        matrix.inverse
        
        ## Return a matrix that is the inverse of 'x'
}
