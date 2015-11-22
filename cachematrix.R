## calculates the inverse of the matrix if it not cached , if cahced returns the cached value 
## and avoids calcualting inverse of it.


## cache the matrix passed

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) n <<- inv
        getinverse <- function() n
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## calls makeCacheMatrix to check if the matrix has been cached if not it calucaltes the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        m <- x$get()
        n <- solve(m, ...)
        x$setinverse(n)
        n
}
