

param <- matrix(nrow=10,ncol=12)
View(param)
param <- as.data.frame(matrix(nrow=10,ncol=12))
colnames(param) <- c("y.h.mu","y.h.sd","y.a.mu","y.a.sd","x.h.mu","x.h.sd","x.a.mu","x.a.sd","al.h.mu","al.h.sd","al.a.mu","al.a.sd")

% y.h.mu - yi_human_mu
% y.a.mu - yi_alien_mu

j <- 1
for(i in 1:10){
c <-1
while(j<31 && c<13)
{

param[i,c] <- mean(t30[which(t30$label==1),j]) 

c <- c+1
param[i,c] <- sd(t30[which(t30$label==1),j]) 

c <- c+1
param[i,c] <- mean(t30[which(t30$label==2),j]) 

c <- c+1
param[i,c] <- sd(t30[which(t30$label==2),j]) 

j <- j+1
c <- c+1
}

}

write.csv(param,file="param.csv")

p_c <- as.data.frame(matrix(nrow=10,ncol=6))
View(p_c)
colnames(p_c) <- c("y.h","y.a","x.h","x.a","a.h","a.a")


% i am just subsetting data.

all_y_h <- t30[which(t30$label==1),c(1,4,7,10,13,16,19,22,25,28)]
all_x_h <- t30[which(t30$label==1),c(2,5,8,11,14,17,20,23,26,29)]
all_alpha_h <- t30[which(t30$label==1),c(3,6,9,12,15,18,21,24,27,30)]

all_y_a <- t30[which(t30$label==2),c(1,4,7,10,13,16,19,22,25,28)]
all_x_a <- t30[which(t30$label==2),c(2,5,8,11,14,17,20,23,26,29)]
all_alpha_a <- t30[which(t30$label==2),c(3,6,9,12,15,18,21,24,27,30)]

% not required =====================================================
mu_y_h <- mean(colMeans(all_y_h))
s_y_h <- sd(colMeans(all_y_h))

mu_x_h <- mean(colMeans(all_x_h))
s_x_h <- sd(colMeans(all_x_h))

mu_a_h <- mean(colMeans(all_alpha_h))
s_a_h <- sd(colMeans(all_alpha_h))

mu_y_a <- mean(colMeans(all_y_a))
s_a_a <- sd(colMeans(all_y_a))

mu_x_a <- mean(colMeans(all_x_a))
s_x_a <- sd(colMeans(all_x_a))

mu_a_a <- mean(colMeans(all_alpha_a))
s_a_a <- sd(colMeans(all_alpha_a))



% computing distribution.
pr <-param[,c(1,3,5,7,9,11)]

c <- 1
while(c<11)
for(i in 1:10)
{

if(c==1)d <- all_y_h
 if(c==2)d <- all_x_h
if(c==3)d <- all_alpha_h
 if(c==4)d <- all_y_a
if(c==5)d <- all_x_a 
 if(c==6)d <- all_alpha_a
 
 
 
p_c[i,c] <- plnorm(pr[i,c], meanlog=log(mean(colMeans(d))), sdlog=log(sd(colMeans(d))))

}

c <- c+1 
}
