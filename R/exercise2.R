
f.obj_transs <- c(3,2,3,5,6,2,8,10,5,9,12,15)
f.con_transs <- matrix (c(1,1,1,rep(0,9),
                           0,0,0,1,1,rep(0,7),
                           rep(0,5),1,1,1,rep(0,4),
                           1,rep(0,7),1,0,1,0,
                           rep(0,9),1,0,1,
                           0,-1,0,1,0,0,-1,0,1,1,0,0,
                           0,0,-1,0,-1,0,0,-1,0,0,1,1),nrow = 7,byrow=TRUE)
f.dir_transs <- c("<=", "<=", "<=", "=", "=","=","=") 
f.rhs_transs <- c(500,400,300,600,600,0,0)
solution_transs$objval
solution_transs$solution