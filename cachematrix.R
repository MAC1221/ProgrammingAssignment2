## makeCacheMatrix and cacheSolve are two functions that work together.
#
# makeCacheMatrix returns a list of functions
# 
# cacheSolve takes the output of makeCacheMatrix as an argument
#       and returns the inverse of a matrix
#
## The inverse for a matrix is computed and cached the first time
#   cacheSolve is called for that matrix.
#  Any subsequent calls of cacheSolve for that matrix will produce
#   the cached inverse without the computation
#
## The chosen matrix is passed to cacheSolve in one of two ways:
#  1)           FUNS_matrix<-makeCacheMatrix(MatrixName)
#               matrix_inverse<-cacheSolve(FUNS_matrix)
#       The inverse matrix of MatrixName is computed and cached
#        at the first call of cacheSolve(FUNS_matrix).
#       Subsequent calls of cacheSolve(FUNS_matrix) will produced 
#        the previously computed and cached inverse.
#  2) PERSONALLY, I can think of no practical use for the second 
#       method but ...
#               FUNS<-makeCacheMatrix()
#               FUNS$set(MatrixName)
#               matrix_inverse<-cacheSolve(FUNS)
#       The inverse matrix of MatrixName will be computed and cached
#         at the first call of cacheSolve(FUNS)
#      Subsequent calls of cacheSolve(FUNS) will produced 
#        the previously computed and cached inverse 
#        so long as the matrix has not been reset
#        with another call to FUNS$set
#
## makeCacheMatrix outputs four functions:
#               1) set: sets the matrix in the environment 
#                       of these functions 
#                       to that provided by the argument
#               2) get: gets the matrix
#               3) setinv: sets the inverse matrix in the evironemnt 
#                       of these functions 
#                       to that provided by the argument
#               4) getinv: gets the inverse matrix
# the matrix may be provided as an optional argument
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## CacheSolve takes as argument the four-function list output
#       from the makeCacheMatrix function 
#       plus any valid arguments for the called functions 
#       and returns the inverse of a matrix.
#
#  The function first checks for a cached inverse
#       and returns the cached inverse if found.
#  If no inverse has been cached, the inverse in computed,
#       cached in the environment of the calling function
#       and returned.
#
#  If the cached inverse is returned a message to that effect
#       is printed
#
#  If the matrix inverse is being computed for the first time
#       and the matrix is not square, a message to that effect
#       is printed and an NaN inverse is returned.
#
#  If the matrix is not invertible, R returns a error.
#
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        if (nrow(data)!=ncol(data)) {
                message("matrix not square")
                inv<-matrix(NaN)
                return(inv)
        }
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
