
ellipses = function(m, CV, probs)
{
  # Compute and plot an ellipse region for bivariate Gaussians, i.e., some ellipse that 
  # has probability probs to contain a 2D Gaussian vector with given parameters.
  # Inputs: 
  #   - m: means
  #   - CV: covariance matrix
  #   - probs: probabilities
  # Source : https://waterprogramming.wordpress.com/2016/11/07/plotting-probability-ellipses-for-bivariate-normal-distributions/
  
  # Coordinates of mean
  b1 = m[1]
  b2 = m[2]
  
  eg = eigen(CV)
  Evec = eg$vectors
  Eval = eg$values
  
  theta = seq(0,2*pi,0.01) # angles used for plotting ellipses
  
  vec.norm = function(v) { sqrt(t(v) %*% v)}
  # compute angle for rotation of ellipse
  # rotation angle will be angle between x axis and first eigenvector
  x.vec = c(1, 0) # vector along x-axis
  cosrotation = t(x.vec) %*% Evec[,1]/(vec.norm(x.vec)*vec.norm(Evec[,1]))
  rotation = pi/2-acos(cosrotation) # rotation angle
  #create a rotation matrix
  R  = matrix(c(sin(rotation), cos(rotation), -cos(rotation), sin(rotation)), 
              nrow=2, ncol=2, byrow = TRUE)
  
  # Create chi squared vector
  chisq = qchisq(probs,2) # percentiles of chi^2 dist df=2
  
  # size ellipses for each quantile
  xRadius = rep(0, length(chisq))
  yRadius = rep(0, length(chisq))
  x = list()
  y = list()
  x.plot = list()
  y.plot = list()
  rotated_Coords = list()
  for (i in 1:length(chisq)) {
    # calculate the radius of the ellipse
    xRadius[i]=(chisq[i]*Eval[1])^.5; # primary axis
    yRadius[i]=(chisq[i]*Eval[2])^.5; # secondary axis
    # lines for plotting ellipse
    x[[i]] = xRadius[i]* cos(theta);
    y[[i]] = yRadius[i] * sin(theta);
    # rotate ellipse
    rotated_Coords[[i]] = R %*% matrix(c(x[[i]], y[[i]]), nrow=2, byrow=TRUE)
    # center ellipse
    x.plot[[i]] = t(rotated_Coords[[i]][1,]) + b1
    y.plot[[i]] = t(rotated_Coords[[i]][2,]) + b2}
  
  xlim = range(x.plot[[i]])
  ylim = range(y.plot[[i]])
  plot(b1,b2, xlab = "X1", ylab = "X2", xlim = xlim, ylim = ylim, cex=1)
  abline(h=0)
  abline(v=0)
  # Plot contours
  for (j in 1:length(chisq)) {
    points(x.plot[[j]],y.plot[[j]], cex=0.1)}
  
  legend("bottomright", c('Ellipse region', paste(probs)))}

set.seed(0)
mat <- matrix(0, nrow=6000, ncol=201)
for (i in 1:6000){
  mat[i,] = rnorm(201,0,1)
}
indices <- seq(1:6000)
frame <- as.data.frame(mat)
frame[1, 199]
y <- mat[,1]
x <- mat[, 2:201]
x

#Dessiner le nuage des points du premier vecteur.
plot(indices ,y, type = "l", col="red")

model <- lm(y~x, data=frame)
summary(model)
p_values = summary(model)$coefficients[,4]
count_non_zero = 0
for (i in 1:length(p_values)){
  if(p_values[i] < 0.05){
    count_non_zero = count_non_zero + 1
  }
}
#Dessiner le nuage des y estimés du premier vecteur.
fittedVlues <- fitted(model)
lines(indices,fittedVlues,col="green")
length(p_values)
count_non_zero


#Question 2
n = 1000
epsilon1 <- rnorm(1000,0,1)
epsilon2 <- rnorm(1000,0,1)
epsilon3 <- rnorm(1000,0,1)
X1 <- epsilon1
X2 <- 3*X1 +epsilon2
Y <- 2 + X1 + X2 + epsilon3
plot(X1,X2)
set.seed(3)
epsilonp1 <- rnorm(10,0,1)
epsilonp2 <- rnorm(10,0,1)
epsilonp3 <- rnorm(10,0,1)
Xp1 <- epsilonp1
Xp2 <- 3*Xp1 +epsilonp2
Yp <-  2 + Xp1 + Xp2 + epsilonp3
Yp

plot(X1,X2) 

Yp
#Modele 1:
model1 = lm(Yp~Xp1)
fitted(model1)

#Modele 2:
model2 = lm(Yp~Xp2)
fitted(model2)

#questionC
model3 =  lm(Yp~Xp1 + Xp2)
model3p = lm(Xp2~Xp1)
summary(model3)$coefficients
summary(model3p)$coefficients

#c1: Comment on the effect of bith predictors and provide two different points of view:
#========1)======:Plus le nombre de predicteurs X_i augmente, plus l'ecart type augamente et donc la précision
#de l'estimation des coefficients du modele de regresseion lineaire diminue
#=======2)=======:la regression sur X1 ou bien sur X2 donne pratiquement une meme estimation unique des coefficients
#ceci est du au fait que X1 dépend de X2  lineairement par le biais du modele 2a.

#c2: Are these points of view consistent:
#la conhérence du point de vue 1) est soutenu par les résultats obtenus : 
  ##La regression lineaire de Y sur X1 donne un ecart type de 0.3953971  pour l'estimation de Beta1 et un ecrat type de 1.2974305 pour La regression lineaire de Y sur X1 et X2.
  ##La regression lineaire de Y sur X2 donne un ecart type de 0.1369557  pour l'estimation de Beta2 et un ecrat type de 0.4519210 pour La regression lineaire de Y sur X1 et X2.

#la conhérence du point de vue 2) est soutenu par les résultats obtenus : 
  ## Betha0_modele3 =  Betha0_modele1 + Betha0_modele2/2
  ## Betha1_modele3 =  Betha1_modele1/2 
  ## Betha2_modele3 = Betha2_modele2/2 
  ## Ces trois equations donne exacteme,t la même estimation du modele 3 (qu'est celui de la regression de Y sur X1 et X2)

#WHAT COULD EXPLAIN THIS : lES ECARTS TYPE S'ADDITIONNENT ET LA PRÉCISION DE L'ESTIMATION DES COEFFS DIMINUE.




#questionD
colone1 = matrix(1, nrow = 10 ,ncol = 1 )
matriceX =matrix(nrow = 10 ,ncol = 3 )
matriceX[,1] = colone1
matriceX[,2] = Xp1
matriceX[,3] = Xp2
matriceX

betha_mu = matrix(nrow = 3 , ncol = 1  )
betha_mu[1,1] = 2
betha_mu[2,1] = 1
betha_mu[3,1] = 1
matriceX_trans = t(matriceX)
matrix_cov = matriceX_trans%*%matriceX
matriInverse = solve(matrix_cov)

#Density betha1_chapeau
Vx <- seq(-10, 10, length=1000)
betha_chapeau1 = dnorm(Vx, betha_mu[2,1] , matriInverse[2,2] )


#Density betha2_chapeau
Vxx <- seq(-5, 5, length=1000)
betha_chapeau2 = dnorm(Vxx, betha_mu[3,1] , matriInverse[3,3] )


#Quantiles de betha1_chapeau en 0.025 et 0.975
quantile0.025 <- qnorm(1 - 0.025/2 , mean = betha_mu[2,1],  sd = matriInverse[2,2] )
quantile0.975 <- qnorm(1 - 0.975 , mean = betha_mu[2,1],  sd = matriInverse[2,2] )

#Quantiles de betha2_chapeau en 0.025 et 0.975
quantile0.025a <- qnorm(1 - 0.025/2 , mean = betha_mu[3,1],  sd = matriInverse[3,3] )
quantile0.975a <- qnorm(1 - 0.975/2 , mean = betha_mu[3,1],  sd = matriInverse[3,3] )

#
densityB1_0.025 =  dnorm(quantile0.025, mean = betha_mu[2,1], sd = matriInverse[2,2]  )
densityB1_0.975 =  dnorm(quantile0.975, mean = betha_mu[2,1], sd = matriInverse[2,2]  )
densityB2_0.025 =  dnorm(quantile0.025a, mean = betha_mu[3,1], sd = matriInverse[3,3]  )
densityB2_0.975 =  dnorm(quantile0.975a, mean = betha_mu[3,1], sd = matriInverse[3,3]  )


#plot Betha1_chapeau:
plot(Vx, betha_chapeau1, type = "l")
polygon(Vx, betha_chapeau1, col = "magenta", border = "black")
segments(quantile0.025, 0,  quantile0.025, densityB1_0.025 )
segments(quantile0.975, 0,  quantile0.975, densityB1_0.975 )
ellipses(betha_mu, matriInverse, 0.5)
#plot Betha2_chapeau
plot(Vxx, betha_chapeau2, type = "l")
polygon(Vxx, betha_chapeau2, col = "purple", border = "black")
segments(quantile0.025a, 0,  quantile0.025a, densityB2_0.025 )
segments(quantile0.975a, 0,  quantile0.975a, densityB2_0.975 )

