### makeCacheMatrix generates a list for different functions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

### Fucntion returns the inverse of the matrix. It first checks if
#it initially verifies whether the inverse has been previously computed.
#if it has, the stored result is retrieved to avoid redundant computation.
#if not, it calculates the inverse and stores the result using the setinverse
#function
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached matrix")
    return(invmatrix)
  }
  matrix <- x$get()
  invmatrix <- solve(matrix)
  x$setinverse(invmatrix)
  return(invmatrix)
}
