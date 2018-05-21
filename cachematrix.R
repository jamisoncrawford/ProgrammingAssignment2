
# R Programming: Week 3, Assignment 2

## R version 3.4.3
## RStudio version 1.1.414

## Description: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
  inv <- NULL
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inverse <- function( solve ) inv <<- solve
  get.inverse <- function() inv
  list( set = set, get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse )
}

## Description: cacheSelve() computes the inverse of the special "matrix" returned by makeCacheMatrix() above. 
## Note: If inverse is already calculated, and matrix unaltered, then cacheSolve() retrieves inverse from cache.

cacheSolve <- function( x, ... ) {
  inv <- x$get.inverse()
  if( !is.null( inv )) {
    message( "getting cached data" )
    return( inv )
  }
  data <- x$get()
  inv <- solve( data , ... )
  x$set.inverse( inv )
  return( inv )
}

## Testing (Note: rnorm() likely, but not guaranteed to generate invertible matrices!)

my_matrix <- matrix( data = rnorm( 81 ),
                     nrow = 9,
                     ncol = 9 )

my_matrix <- makeCacheMatrix( matrix( data = rnorm( 81 ),
                                      nrow = 9,
                                      ncol = 9 ))
my_matrix$get()

cacheSolve( my_matrix )
