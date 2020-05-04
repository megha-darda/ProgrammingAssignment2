## Put comments here that give an overall description of what your

## Write a short comment describing this function

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat <- function(inver) mat <<- inver
  getmat <- function() mat
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

## Write a short comment describing this function

##The following function calculates inverse of matrix
##created with the above function. However, it first 
##checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the
##matrix using the solve function and sets the inverse in the 
##cache via the setmat function.



cacheSolve <- function(x, ...) {
  mat <- x$getmat()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmat(mat)
  mat
}

#Trial:
#> q<-cbind(c(1,2),c(4,5))
#> z<-makeCacheMatrix(q)
#> z$get()
#[,1] [,2]
#[1,]    1    4
#[2,]    2    5
#> cacheSolve(z)
#[,1]       [,2]
#[1,] -1.6666667  1.3333333
#[2,]  0.6666667 -0.3333333
#> cacheSolve(z)
#getting cached data
#[,1]       [,2]
#[1,] -1.6666667  1.3333333
#[2,]  0.6666667 -0.3333333
