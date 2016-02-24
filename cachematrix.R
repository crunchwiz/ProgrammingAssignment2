## makeCacheMatrix returns a list of functions, all of which handles the caching and 
## retrieving of both the original matrix and inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    set <- function(m) {
        x <<- m
        invertedMatrix <<- NULL
        print("In Set baby")
    }
    get <- function() { x }
    setInvertedMatrix <- function(m) { invertedMatrix <<- m }
    getInvertedMatrix <- function() { invertedMatrix }
    list( setMatrix = set, getMatrix = get, setInvertedMatrix = setInvertedMatrix, 
          getInvertedMatrix = getInvertedMatrix )
    
}


## cacheSolve implements the solve(x, ...) method that calculates a matrix inverse.  
## The inverse is stored in cache and returned by the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInvertedMatrix()
    if(!is.null(invMatrix)) {
        message("getting inverted matrix: ")
        return(invMatrix)
    } else {
        origMatrix <- x$getMatrix()
        m <- solve(origMatrix, ...)
        x$setInvertedMatrix(m)
        m
    }
}
