makeCacheMatrix <- function(x = matrix()) {
  invertedmatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invertedmatrix <<- inverse
  getinverse <- function() invertedmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  invertedmatrix <- x$getinverse()
  if (!is.null(invertedmatrix)) {
    message("getting cached data")
    return(invertedmatrix)
  }
  data <- x$get()
  invertedmatrix <- solve(data, ...)
  x$setinverse(invertedmatrix)
  invertedmatrix
  
}

# matrixgenerator --> a function to generate a 
# new random matrix easily which u can then use to test the caching system

matrixgenerator <- function (amountofmatrixelements) {
  matrixvalues <- rnorm(amountofmatrixelements)
  matrixdimension <- sqrt(amountofmatrixelements)
  matrixgenerated <- matrix(matrixvalues,matrixdimension)
  matrixgenerated
}



# how to test:
# drei <- matrixgenerator(16)
# dreimat <- makeCacheMatrix(drei)
# cacheSolve(dreimat)
# cacheSolve(dreimat) (the second execution will trigger the cache)
