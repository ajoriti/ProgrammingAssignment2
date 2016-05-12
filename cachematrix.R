## The following functions together create the inverse of a supplied square
## matrix x.    

## makeCacheMatrix creates a set of functions that can be used to create
## the inverse and store it in the cache.

makeCacheMatrix <- function(x = matrix()) { 
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) Inv <<- solve(x)
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve finds the inverse of the matrix x unless
## the cache already has the inverse.  In which case, it grabs it from there 
## instead.

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get() ## grabs x from a
        Inv <- solve(a = data, ...) ## Solves the Inverse if needed
        x$setinverse(Inv) 
        Inv
}

## Note: To get the value into the cache. Run makeCacheMatrix and store it in an
## object like "a".  Then run a$get(), a$set(x), a$setinverse(x), and 
## a$getinverse().  In particular, a$setinvese stores in the Inverse in Inv.
## This allows cacheSolve(a) to skip the inverse solve step entirely and pull
## the inverse matrix out of the cache!