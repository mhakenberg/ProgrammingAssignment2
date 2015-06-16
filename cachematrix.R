## Put comments here that give an overall description of what your
## functions do

## Aknowlegments
## Both fuctions are based on examples posted on Coursera R Programming Assignment2       
## webpage  - they were modified  and tested by me  

## My functions prevent from repeating time-costly operation of inverisng the same matrix many times - storing 
## previously calculated inverses in a matrix-cache object

## Write a short comment describing this function
## makeCacheMatrix creates a special list where input matrix values (from input parameter(matrix) x) and its inverse 
## matrix (if previously computed by cacheSolve) are stored, ready for access by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv  <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve checks for inverse matrix stored in a list created by makeCacheMatrix for this specific matrix 
## and if there is no inverse matrix stored then reads matrix stored in the list, 
## computes the inverse and stores it in the list for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
