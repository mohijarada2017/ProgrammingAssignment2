# Done by Mohee Jarada on Oct 26, 2017:

# To read about Matrix inverse, good materials to start with:
# https://www.mathsisfun.com/algebra/matrix-inverse.html
# https://www.statmethods.net/advstats/matrix.html

# This function creates a special "matrix" object that can cache its inverse:
#  assume that the matrix supplied is always invertible
makeCacheMatrix <- function(pMatrixData = matrix()) {
  # Inverse matrix global env reference to store for caching:
  matInversRef <- NULL
  
  set <- function(y) {
    pMatrixData <<- y
    matInversRef <<- NULL
  }
  
  get <- function() pMatrixData
  
  setInverseMat <- function(solve) matInversRef <<- solve
  getInverseMat <- function() matInversRef
  
  # return a list of 4 functions like in OOP style:
  list(set = set, get = get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)
} # end makeCacheMatrix()

# /////////////////////////////////////////////////////////////////

# Below function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix() above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache (global environment)
cacheSolve <- function(x, ...) {
  matInversRef <- x$getInverseMat()
  
  # Check if the inverse matrix has already been calculated and 
  # the matrix size has not changed:
  if(!is.null(matInversRef) & length(x$getInverseMat()) == length(matInversRef)) {
    message("getting cached data...")
    return(matInversRef)
  }
  
  data <- x$get()
  matInversRef <- solve(data, ...)
  x$setInverseMat(matInversRef)
  
  matInversRef
} # end cacheSolve()

# 1st case: Testing it with 2x2 identity matrix and check inverse resulted cached matrix:
retMat <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
cacheSolve(retMat)
cacheSolve(retMat)
print("###################################################x")
# 2nd case: Testing it with 2x2 identity matrix and then change its size to be 3x3
retMat <- NULL
retMat <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
cacheSolve(retMat)
retMat <- makeCacheMatrix(matrix(c(4, 2, 7, 6, 1, 5, 8, 10, 3), 3, 3))
cacheSolve(retMat)
