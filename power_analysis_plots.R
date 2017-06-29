## A simple one-sample one-sided t-test Power Calculation 

# inputs
n <- 20 # Sample Size
sigma <- 1 # Estimate of sigma
delta <- 1 # Practically sig. difference
effect <- delta/(sigma/sqrt(n)) # Effect Size
alpha = .05 # Confidence Level

## Put sidedness into function
# alt_sided = "two"

n_grid <- 10000
lim_a <- -4
lim_b <- 8

## Build grids
grid_x <- seq(from = lim_a,
              to = lim_b,
              length.out = n_grid)
grid_x1 <- seq(qt((1-alpha), n-1, 0), lim_b, length.out = 400)
grid_x2 <- seq(lim_a, qt((1-alpha), n-1, 0), length.out = 400)

plot(x = grid_x,
     y = dt(grid_x, n-1, 0),
     type = "l",
     xlim = c(lim_a, lim_b),
     ylim = c(0, 1.2*max(dt(grid_x,n-1,0))),
     ylab = "P(T = t)",
     xlab = "t")

text(0, 1.1*max(dt(grid_x, n-1, 0)), "H0")
text(effect, 1.1*max(dt(grid_x, n-1, effect)), "Ha")
text(effect, .5*max(dt(grid_x, n-1, effect)), "Power")
polygon(x = c(grid_x1, rev(grid_x1)),
        y = c(dt(grid_x1, n-1, 0), rep(0, length(grid_x1))),
        col = rgb(.2, .8, .2, alpha = .4),
        border = "black")
lines(x = grid_x,
      y = dt(grid_x, n-1, effect))
polygon(x = c(grid_x1, rev(grid_x1)),
        y = c(dt(grid_x1, n-1, effect), rep(0, length(grid_x1))),
        col = rgb(.1, .1, .9, alpha = .2))
polygon(x = c(grid_x2, rev(grid_x2)),
        y = c(dt(grid_x2, n-1, effect), rep(0, length(grid_x2))),
        col = rgb(.9, .1, .1, alpha = .2))
abline(v = qt(.95, n-1, 0),
       col = rgb(1,0,0),
       lty = 2)
text(1.1*qt(.95, n-1, 0), .5*dt(qt(.95, n-1, 0), n-1,0 ), expression(paste(alpha)))
text(.8*qt(.95, n-1, 0), .4*dt(qt(.95, n-1, 0), n-1,effect ), expression(paste(beta)))

n_grid <- 2:200
plot(x = n_grid, 
     y = pt(qt((1-alpha),n_grid-1,0), n_grid-1, effect, lower.tail = FALSE),
     type = "l")
points(n, 
       pt(qt((1-alpha),n-1,0), n-1, effect, lower.tail = FALSE),
       pch = 20,
       col = rgb(.7, .1, .1))

pt(qt((1-alpha),n-1,0), n-1, effect, lower.tail = FALSE)
pwr.t.test(n = n,
           d = delta,
           sig.level = alpha,
           type = "one.sample",
           alternative = "greater")


s <- 2
n <- 20
ncp <- 1.5/(s/sqrt(n))
t <- qt(0.975,df=n-1)
pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp) # 0.1111522
1-(pt(t,df=n-1,ncp=ncp)-pt(-t,df=n-1,ncp=ncp)) # 0.8888478


library(pwr)
str(pwr.t.test(n = 20,
           d = 1,
           sig.level = .05,
           type = "one.sample",
           alternative = "greater")
)

effect/(sigma/sqrt(n))

n
pwr <- pt(qt((1-alpha), n-1,0),
n-1, 
(delta/(sigma/sqrt(n))),
lower.tail = FALSE)

tb1 <- cbind(c("Sigma", "Delta", "Sample Size", "ncp", "Alpha", "Power"),
             c(sigma, delta, n, effect, alpha, pwr))
sigma
delta
n
alpha
pwr
tb1 <- as.table(tb1)
colnames(tb1) <- c("", "")
rownames(tb1) <- rep("", 6)
tb1

