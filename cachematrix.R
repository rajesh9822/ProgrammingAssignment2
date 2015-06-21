## This script has two functions makeCacheMatrix and Cachesolve
## makeCacheMatrix function creates a special Matrix object which can cacheits inverse
## cacheSolve function computes the inverse of special matrix
## if inverse is already calculated it ruturns inverse from cache

## makeCacheMatrix
## Creates Special Matrix which can store its inverse
## Does the following function
## 1.getmat() : gets the matrix
## 2.setmat() : sets the new matrix
## 3.getinv() : gets the inverse
## 4.setinv() : sets the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }    
        
        getmat <- function() x
        
        setinv <- function(invmat) inv <<- invmat
        
        getinv <- function() inv
        
        list(setmat = setmat, 
             getmat = getmat,
             setinv = setinv,
             getinv = getinv)
        
}
##cacheSolve
## Computes the inverse of Special Matrix
## Does the following function
## 1.gets the inverse from cache if its already computed for same matrix
## 2.computes the inverse matrix and stores in special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("inverse is already cached getting it....")
                return(inv)
        }
        
        data <- x$getmat()
        inv <- solve(data, ...)
        message("computing inverse matrix...")
        x$setinv(inv)
        inv
        
}
