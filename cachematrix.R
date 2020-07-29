## Hi im Luisa and this is my first atempt to do the Peer-graded 
## Assignment: Programming Assignment 2: Lexical Scoping

## I would love if you guys can say something about the way I indented my code, 
## and if the coments I made are good, because I think I strugle the most with that.
## thanks a lot :) 

## makeCacheMatrix: creates a special "matrix" that can cache its inverse
## this function gets the values for an inverse matrix, in other words, 
## creates an invertible matrix 

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
    S <- NULL
    ST <- function(y) {
        x <<- y
        S <<- NULL
    }
    GET <- function() x
    SINV <- function(solve) S <<- solve
    GINV <- function() S
    list(ST = ST, GET = GET, SINV = SINV, GINV = GINV)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated, cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    S <- x$GINV()
    if(!is.null(S)) {
        message("getting inversed matrix")
        return(S)
    }
    DATA <- x$get()
    S <- solve(DATA, ...)
    x$SINV(S)
    S
}
