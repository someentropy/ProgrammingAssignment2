###################################
##  Solution to Assignment 2
###################################  
##  Functions are built to cache the
##  results of the solve() function
##  on a square matrix
###################################  

##  Function set (list) to hold the matrix 
##  and cache its inverse using solve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(yourMatrix) {
        x <<- yourMatrix
        inv <<- NULL
    }
    
    get <- function() x 
    
    setInv <- function(solve) {
        inv <<- solve
    }
    
    getInv <- function() {
        inv
    }
    
    list(   
        set = set, 
        get = get,
        setInv = setInv,
        getInv = getInv
    )
}

##  Call against function list of type makeCacheMatrix
##  to solve and cache a square matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        inv
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
