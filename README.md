# check matrix equality
```
Posdef <- function (n, ev = runif(n, 0, 10)) 
{ 
  Z <- matrix(ncol=n, rnorm(n^2)) 
  decomp <- qr(Z) 
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp) 
  d <- diag(R) 
  ph <- d / abs(d) 
  O <- Q %*% diag(ph) 
  Z <- t(O) %*% diag(ev) %*% O 
  return(Z) 
} 

Sigmab<-Posdef(5)
H<-Posdef(5)

solve(Sigmab)-solve(Sigmab)%*%solve(H+solve(Sigmab))%*%solve(Sigmab)
H-H%*%solve(H+solve(Sigmab))%*%H

solve(Sigmab)%*%solve(H+solve(Sigmab))%*%H

Sigmae<-Posdef(10)
Sigmab<-Posdef(5)
X<-matrix(runif(50),10,5)

# Sigmae<-diag(runif(10))
# Sigmab<-matrix(NA,5,5)
# Sigmab<-0.8^abs(row(Sigmab)-col(Sigmab))
# X<-matrix(runif(50),10,5)

A<-solve(Sigmab)-solve(Sigmab)%*%solve(t(X)%*%solve(Sigmae)%*%X+solve(Sigmab))%*%solve(Sigmab)
B<-t(X)%*%solve(X%*%Sigmab%*%t(X)+Sigmae)%*%X
H<-t(X)%*%solve(Sigmae)%*%X
C<-H-H%*%solve(H+solve(Sigmab))%*%H
D<-solve(Sigmab)%*%solve(H+solve(Sigmab))%*%H
E<-solve(Sigmab)-solve(Sigmab)%*%solve(H+solve(Sigmab))%*%solve(Sigmab)

A-B
A-C
A-D
A-E
```

```
Sigmae<-diag(runif(10))
Sigmab<-matrix(NA,5,5)
Sigmab<-0.8^abs(row(Sigmab)-col(Sigmab))
X<-matrix(runif(50),10,5)

A<-solve(Sigmab)-solve(Sigmab)%*%solve(t(X)%*%solve(Sigmae)%*%X+solve(Sigmab))%*%solve(Sigmab)
B<-t(X)%*%solve(X%*%Sigmab%*%t(X)+Sigmae)%*%X

A-B
```
