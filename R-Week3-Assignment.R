makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix;
        getInverseMatrix <- function() m
        list(set = set, 
             get = get, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x) {
        m <- x$getInverseMatrix()
        if (!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
