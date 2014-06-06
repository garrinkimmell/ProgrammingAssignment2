## This module provides a matrix data structure which memoizes its inverse. 


## makeCacheMatrix creates a matrix wrapper that memoizes its inverse. 
## The underlying matrix should be accessed by the `get` operation, and 
## the inverse should be accessed using the `solve` operation. There is no
## `set` operation, because that's just silly. To `set`, just create a new 
## matrix wrapper using `makeCacheMatrix`.
makeCacheMatrix <- function(x = matrix()) {
  e <- new.env()
  e$get <- function() x
  e$inverse <- function() {
      message("Solving for the first time")
      inv <- solve(x)
      cinv <- makeCacheMatrix(inv)
      e$inverse <- function () {  
        message("Second time through")
        cinv
      }
      cinv$inv <- function () { e }
      
      cinv
  }
  e
}


## cacheSolve returns the inverse of a matrix created with the `makeCachMatrix` 
## wrapper. Seems unnecessary, because it just wraps the `solve` function in `x`.
cacheSolve <- function(x, ...) {
   x$inverse()
}


## Tests cases
test <- function () {
  m <- matrix(1.1:9.1, nrow=3,ncol=3)
  m[1,1] <- 1.0
  cm1 <- makeCacheMatrix(m)
  
  # Creating a cached matrix, then getting it, should be identity.
  message("Testing get-over-create")
  ok1 <- m == cm1$get()
  message(c("create-get-id ", ok1))
  
  # Creating a cached matrix, then solving twice, should only show the 
  # 'Solving for the first time' message once.
  message("Testing caching ")
  cm2 <- makeCacheMatrix(m)
  cacheSolve(cm2)
  cacheSolve(cm2)
  
  
  # Inverse/inverse composition should be identity.
  message("Testing inverse-inverse-identity ")
  cm3 <- makeCacheMatrix(m)
  i3 <- cacheSolve(cm3)
  ii3 <- cacheSolve(i3)
  
  # Can't test equality directly, due to numerical imprecision across the `solve` function.
  # So we just test to see if the two matrices are equivalent within some epsilon.
  
  # ok2 <- cm3$get() == ii3
  epsilon <- 1.0e-13
  ok2 <- all(cm3$get() - ii3$get() < epsilon)
    
  message(c("inv-inv-id ", ok2))
  

}

