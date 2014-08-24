## makeCacheMatrix function takes a matrix and inverses it. If the inverse
## is available in the cache memory then the cached inverse is passed else the 
## function cacheSolve is called to calculate the inverse of the matrix


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    setmatrix <- function(y) {
        x <<- y
        m <<- matrix()
    }
    getmatrix <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    ##This function returns the following list
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    comp<-matrix()   ## A null matrix to compare
    ## Compare the matrices if inverse hasn't been calculated
    ## If inverse has been calculated, check if the matrices are same
    if((!(identical(comp,m)))  && identical(orig,x$getmatrix())){
        message("getting cached data")
        return(m)
    }
    
    ## If inverse wasn't calculated before, calculate now
    data <- x$getmatrix()
    m <- solve(data, ...)
    message("calculating inverse")
    x$setinverse(m)
    orig <<- data   ## Store the original matrix for comparison
    m
}
