
## makeCacheMatrix() function sets and gets the matrix and its inverse
## Creates a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        p<-NULL
        set<-function(y){
                x<<-y
                p<<-NULL
                }
        get<-function() x
        setInv <- function(inverse) j <<- mean
        getInv <- function() j
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## cacheSolve() function solves and gives the inverse of the matrix that is given by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        p <- x$getInv()
        if(!is.null(p)) {
                message("getting cached data")
                return(p)
        }
        data_matrix <- x$get()
        p <- mean(data_matrix, ...)
        x$setInv(p)
        p
}
