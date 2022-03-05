#SIMULATION 
#####

install.packages("rgl")
library(rgl)
options(rgl.printRglwidget = TRUE)

generateTorus <- function(n, R, r) {
  u <- seq(0, 2*pi, length.out = n)
  v <- seq(0, 2*pi, length.out = n)
  
  x <- cos(v)*(R+r*cos(u))
  y <- sin(v)*(R+r*cos(u)) 
  z <- r*sin(u)
  
  data <- expand.grid(u=u, v=v)
  data <- transform(data, x = x, y =y,z = y)
  
  X <- cbind(data$x, data$y, data$z)
  
  return(X)
}
X <- generateTorus(1000, 1/2, 3)
plot3d(X)
t <- seq(0, 2*pi, length.out=50); 
u <- seq(0, 2*pi, length.out=50); 
tu<-expand.grid(t=t,u=u)
R <- 6; 
r <- 3; 
tu <- transform(tu, 
                x = cos(t)*(R+r*cos(u)), 
                y = sin(t)*(R+r*cos(u)),
                z = r*sin(u)
)
rr<-c(-10,10)
cloud(z~x+y, tu, xlim=rr, ylim=rr, zlim=rr, screen=list(y=20))


