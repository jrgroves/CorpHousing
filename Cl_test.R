mpi.master <- function(y, rho, Niter) {
  
  library(Rmpi)
  
  mpi.spawn.Rslaves()
  
  mpi.bcast.Robj2slave(y)
  mpi.bcast.Robj2slave(rho)
  mpi.bcast.Robj2slave(Niter)
  
  theta.startmat <- matrix(c(5, 5, -5, 5, 5, -5, -5, -5), 4, 2, byrow=TRUE)
  mpi.bcast.Robj2slave(theta.startmat)
  
  mpi.remote.exec(mpi.slave())
}

mpi.slave <- function() {
  
  ind <- mpi.com.rank()
  
  thetamat <- matrix(0, Niter, 2)
  thetamat[1,] <- theta.startmat[ind,]
  
  for (i in 2: Niter){
    thetamat[i,2] <- rnorm(1, y[2] + rho*(thetamat[i-1,1] - y[1]), sqrt(1-rho^2))
    
    thetamat[i,1] <- rnorm(1, y[1] + rho*(thetamat[i,2] - y[2]), sqrt(1-rho^2))
  }
  
  write.table(thetamat, file=paste("output.", ind, sep=""), quote = FALSE, sep = "\t",
              row.names = FALSE, col.names = FALSE)
}

y <-c(0,0)
rho <- 0.8
Niter <- 5000

start <- Sys.time()
mpi.master(y, rho, Niter)
Sys.time() - start

mpi.close.Rslave(dellog = FALSE)
mpi.quit()