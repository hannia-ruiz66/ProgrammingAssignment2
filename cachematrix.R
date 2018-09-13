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
