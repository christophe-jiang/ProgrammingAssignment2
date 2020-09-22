## The R script contains 2 functions : makeCacheMatrix that puts in cache the inverse of a matrix
## cacheSolve that returns the inverse of a matrix if already computed, or computes the inverse of a matrix otherwise


## this function creates a special matrix object that can catch its inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    
    ## return a list of elements 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## check if inverse of 'x' already in cache
    if (!is.null(inv)) {
        message("getting catched data")
        print(inv)
    }
    
    ## else, compute the inverse of matrix
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
