## Return a list as an encapsulation of cached a matrix that expose functions
## to set and get the matrix passed as parameter

makeCacheMatrix <- function(x = matrix()) {
    ## init cached matrix
    m <- NULL
    ## return initial matrix passed as parameter to makeCacheMatrix() 
    get <- function() x
    ## set cached matrix to parameter solve
    setSolve <- function(solve) m <<- solve
    ## return cached matrix
    getSolve <- function() m
    ## return the 'wrapper' for the cached matrix
    list( 
        get = get,
        setSolve = setSolve,
        getSolve = getSolve)
}


## Return the inverse of an invertible matrix 'x'

cacheSolve <- function(x, ...) {
    ## get cached solution
    m <- x$getSolve()
    if(is.null(m)) {
        ## not solved
        ## retrieve matrix
        data <- x$get()
        ## solve the inverse
        m <- solve(data)
        ## cache the solution
        x$setSolve(m)
    } else {
        print("return cache result")
    }
    ## Return inverse of 'x'
    m
}

##
##  Example use
##  > y <- makeCacheMatrix(rbind(c(1,-1/4),c(-1/4,1)))
##  > cacheSolve(y)
##
