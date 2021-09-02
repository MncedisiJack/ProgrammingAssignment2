## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cachesolve <- function(x, ...) { 
  i<- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinv(i)
  i
  
}

##Testing the Inverse Function

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getinv()

cachesolve(my_matrix)

my_matrix$getinv()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

my_matrix$get()

my_matrix$getinv()

cachesolve(my_matrix)

my_matrix$getinv()

