`ht` <- function(x){ t(Conj(x)) }

`cprod` <- function(x,y=NULL){
    if(is.null(y)){
        return(crossprod(Conj(x),x))
    } else {
        return(crossprod(Conj(x),y))
    }
}

`tcprod` <- function(x,y=NULL){
    if(is.null(y)){
        return(tcrossprod(x,Conj(x)))
    } else {
        return(tcrossprod(x,Conj(y)))
    }
}

`quad.form.chol` <- function (chol, x){
        jj <- cprod(chol, x)
        cprod(jj, jj)
}

`quad.form` <- function (M, x){ crossprod(crossprod(M,Conj(x)),x) }

`quad.form.inv` <- function (M, x){ cprod(x, solve(M, x)) }

`quad3.form_ab` <- function(M,left,right){ crossprod(crossprod(M, Conj(left)), right) }
`quad3.form_bc` <- function(M,left,right){ cprod(left, (M %*% right)) }
`quad3.form` <- function(M,left,right){
    left <- as.matrix(left)
    right <- as.matrix(right)
    if(ncol(left) < ncol(right)){
        quad3.form_ab(M,left,right)
    } else {
        quad3.form_bc(M,left,right)
    }
}

`quad3.form.inv` <- function(M,left,right){ cprod(left, solve(M, right)) }

`quad3.tform_ab` <- function(M,left,right){ tcprod(left %*% M,right)}
`quad3.tform_bc` <- function(M,left,right){ tcrossprod(left, tcrossprod(Conj(right), M)) }
`quad3.tform` <- function(M,left,right){
    if(nrow(left) < nrow(right)){
        quad3.tform_ab(M,left,right)
    } else {
        quad3.tform_bc(M,left,right)
    }
}
`quad.tform` <- function(M,x){ tcrossprod(x, tcrossprod(Conj(x), M)) }

`quad.tform.inv` <- function(M,x){ quad.form.inv(M, ht(x)) }

`quad.diag` <- function(M,x){ colSums(crossprod(M, Conj(x)) * x) }

`quad.tdiag` <- function(M,x){ rowSums(tcrossprod(Conj(x), M) * x) }

`quad3.diag` <- function(M,left,right){ colSums(crossprod(M, Conj(left)) * right) }

`quad3.tdiag` <- function(M,left,right){ colSums(t(left) * tcprod(M, right)) }

`quad.trace` <- function(M,x){ sum(crossprod(M, Conj(x)) * x) }

`quad.ttrace` <- function(M,x){ sum(tcrossprod(Conj(x), M) * x) }

cp <- cprod
tcp <- tcprod
qf <- quad.form
qfi <- quad.form.inv
q3 <- quad3.form
q3i <- quad3.form.inv

q3t <- quad3.tform
qt <- quad.tform
q3i <- quad.tform.inv
qd <- quad.diag
qtd <- quad.tdiag
q3d <- quad3.diag
q3td <- quad3.tdiag

qtr <- quad.trace
qttr <- quad.ttrace
