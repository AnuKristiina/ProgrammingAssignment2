##The first function, makeVector creates a special "vector", which is really a list containing a function to

##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean

makeVector <- function(x=numeric()){
  m <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  getevn <- function() environment()
  list(set=set, get=get, setmean=setmean, getmean=getmean, getevn=getevn)
}
##The following function calculates the mean of the special "vector" created with the above function. 
##However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the 
##cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the 
##mean in the cache via the setmean function.

cachemean<-function(x,...){
  m <- x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##Matrix inversion is usually a costly computation and their may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
##inversion that we will not discuss here). Your assignment is to write a pair of functions that 
##cache the inverse of a matrix.

##Write the following functions:
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #Computing the inverse of a square matrix can 
                      ##be done with the solve function in R. For example, 
                      ##if X is a square invertible matrix, then solve(X) returns its inverse.
  x$setinverse(inv)
  inv
}

### Example
newmatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
newmatrix$get()         # Returns original matrix

## More testing of the code
cacheSolve(newmatrix) # Computes, cahces and returns matrix inverse
newmatrix$getinverse() # Returns matrix inverse
cacheSolve(newmatrix) # Returns cached matrix inverse using prev. computed matrix inverse
newmatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(newmatrix)   # Computes, caches, and returns new inverse matrix 
newmatrix$get()         # Returns matrix
newmatrix$getinverse()  # Returns inverse matrix
