## The Inverse of matrix can be fetched from cache, when its value is calculated for once


#Function makeCacheMatrix creates a list of 4 objecs namely
#   set
#   get
#   setInv
#   getInv


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


#Function cacheSolve does
#       takes the lists created from the makeCacheMatrix as argument
#       Checks if the inverse is already avaible, if so gets it from cache
#       If not calculates the inverse of the matrix
#       And stores it in the cache

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
