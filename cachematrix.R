## This function creates a special "matrix" object that can cache its inverse.
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function()
                x
        setInverseMatrix <-
                function(Inv)
                        matrixinverse <<- Inv
        getInverseMatrix <- function()
                matrixinverse
        list(
                set = set,
                get = get,
                setInverseMatrix = setInverseMatrix,
                getInverseMatrix = getInverseMatrix
        )
}

# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                matrixinverse <- x$getInverseMatrix()
                if(!is.null(matrixinverse)) {
                        message("getting cached data")
                        return(matrixinverse)
                }
                data <- x$get()
                matrixinverse <- solve(data, ...)
                x$setInverseMatrix(matrixinverse)
                matrixinverse
}


# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }