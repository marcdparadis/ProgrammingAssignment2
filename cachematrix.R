
## Below are two functions that work together to find and cache the inverse of a matrix
## Once cached, if the same matrix is entered, the inverse will be returned from cache

## This function makes a special matrix object that can store (cache) its inverse

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinv <- function(solve) M <<- solve #solve might need to be replaced with inverse here
  getinv <- function() M
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Finds the inverse of the special matrix object above, if the inverse has already been
## cached it returns the cache, otherwise the inverse is calculated

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  M <- X$getinv()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setinv(M)
  M
}


##Testing

test.matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test.matrix
test.matrix$get()
test.matrix$getinv()

cacheSolve(test.matrix)
cacheSolve(test.matrix)
