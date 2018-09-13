## Put comments here that give an overall description of what your
## functions do
## A maxtrix inversion is usually costly, especially when running inside of a loop. 
## The following functions can compute and cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll retrieves the inverse from the cache directly.

{makeCacheMatrix <- function(x = matrix()) { ## i.e. @x: a square invertible matrix
                                   ## define the argument with default mode of "matrix"
  i <- NULL                        ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {             ## define the set function to assign new
    x <<- y                        ## value of matrix in parent environment
    i <<- NULL                     ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x              ## define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) i <<- inverse   ## assigns value of inv in parent environment
  getinverse <- function()                        ## gets the value of inv where called
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)                   ## you need this in order to refer to the functions with the $ operator
}

}

## Write a short comment describing this function:
## Return a matrix that is the inverse of 'x'
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {     
                                      ## @x: output of makeCacheMatrix()
                                      ## return: inverse of the original matrix input to makeCacheMatrix()
  i <- x$getinverse()                 ## if the inverse has already been calculated
  if (!is.null(i)) {                  ## get it from the cache and skips the computation.
    message("getting cached data")
    return(i)                         ## return old result(i) directly 
  }
  data <- x$get()          ## otherwise, get the uncalculated matrix
  i <- solve(data, ...)    ## sets the value of the inverse in the cache via the setinv function.
  x$setinverse(i)          ## reassign inverse matrix 
  i                        ## print the inverse matrix
}
        
## Testing: To test out these functions. I wrote a function called test(), which takes in any invertible matrix, 
##calculates its inverse twice using the above functions, and prints out the times it takes for both runs. 
##The first run should take longer than the second run because it actually calculates the inverse while the second run only does a look-up from the cache.

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}
