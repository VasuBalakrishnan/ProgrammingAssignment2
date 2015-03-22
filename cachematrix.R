## `makeCacheMatrix` creates a special "matrix", which is a list containing a
## function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(get = get, set = set,
       setInverse = setInverse, getInverse = getInverse)
}


## `cacheSolve` calculates inverse of matrix created using `makeCacheMatrix`.
## The function checks if the inverse has already been calculated. If so, it
## gets the inverse from cache and skips the computation. Otherwise, it 
## calculates the inverse and sets the value of inverse in cache using the
## `setInverse` function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message('geting cached data')
    inv
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
