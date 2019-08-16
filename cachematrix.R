## The following two functions aim to simplify the computation process
## and to save the memory space by caching the inverse of a matrix.

## The makecachematrix function creates a 'matrix',which is really 
## a list containing a function to
## 1. Set the matrix  2.Get the matrix  
## 3. Set the inverse of the matrix  4.Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function is going to calculate the inverse matrix of the
## matrix created above. It will firstly check if the result has
## been calculated. If so, it will get the inverse from the cache. 
## Otherwise,it will calculate the inverse matrix by solve function
## and assignthe matrix to the cache by setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$ getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, diag(nrow(data)),...)
  x$ setinverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}



