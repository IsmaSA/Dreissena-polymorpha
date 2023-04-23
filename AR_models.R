## Auto-regression model 


setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")

df <- read_excel("Bivalves.xlsx")
df<- df %>% filter(taxon== "dreissena polymorpha")

df <- read_excel("D_Cohen.xlsx", sheet = "Hoja1")
unique(df$Country)

df1 <- df %>% filter(Country =="Netherlands")
df2 <- df %>% filter(Country =="Denmark")
df3 <- df %>% filter(Country =="Hungary")
df4 <- df %>% filter(Country =="Uk") 
df5 <- df %>% filter(Country =="France")
df6 <- df %>% filter(Country =="Germany") 
df7 <- df %>% filter(Country =="Latvia")
df8 <- df %>% filter(Country =="Europe")

df1 <- df %>% filter(country =="France")
df2 <- df %>% filter(country =="Germany")
df3 <- df %>% filter(country =="UK")
df4 <- df %>% filter(country =="Hungary")
df5 <- df %>% filter(country =="Netherlands")
df6 <- df %>% filter(country =="Switzerland")
df7 <- df %>% filter(country =="Latvia")
df8 <- df %>% filter(country =="Denmark")
df9 <- df %>% filter(country =="Spain")


df8$D<-as.numeric(df8$D)


m1 <- gamm(D ~ s(year),data = df8) #without family=nb() for nice plot , ,  family=nb(link=log)
summary(m1$gam)
## ...so fit the AR1
m2 <- gamm(D ~ s(year),data = df8,
           correlation = corARMA(form = ~ 1|year, p = 1))
## ...and fit the AR2
m3 <- gamm(D ~ s(year),data = df8,
           correlation = corARMA(form = ~ 1|year, p = 2))

summary(m3$gam)
anova(m1$lme, m2$lme, m3$lme)
#Model df       AIC       BIC   logLik   Test   L.Ratio p-value
#m1$lme     1  4 -273.6235 -261.2978 140.8117                         
#m2$lme     2  5 -299.7355 -284.3285 154.8678 1 vs 2 28.112063  <.0001
#m3$lme     3  6 -298.7174 -280.2290 155.3587 2 vs 3  0.981852  0.3217




plot(m1$gam, residuals = TRUE, pch = 19, cex = 0.75)



with(df5, tsDiagGamm(m1, timevar = year, observed = D))



plot(D ~ year, data = df5, type = "p", ylab = ylab)
pdat <- with(df5,
             data.frame(year = seq(min(year), max(year),
                                   length = 200)))
p1 <- predict(m1$gam, newdata = pdat)
p2 <- predict(m2$gam, newdata = pdat)
lines(p1 ~ year, data = pdat, col = "red")
lines(p2 ~ year, data = pdat, col = "blue")
legend("topleft",
       legend = c("Uncorrelated Errors","AR(1) Errors"),
       bty = "n", col = c("red","blue"), lty = 1)


Deriv <- function(mod, n = 200, eps = 1e-7, newdata) {
  if(isTRUE(inherits(mod, "list")))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
  newD <- newD + eps
  X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  # number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    ## Xi <- Xp * 0 ##matrix(0, nrow = Xp.r, ncol = Xp.c)
    ## J <- bs.dims[i]
    ## Xi[,(i-1)  J + 1:J + 1] <- Xp[,(i-1)  J + 1:J +1]
    ## df <- Xi %*% coef(mod)
    ## df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    ## lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  return(lD)
}


m2.d <- Deriv(m1, n = 7264)


plot(m2.d, sizer = TRUE, alpha = 0.01)
dev.off()

df7
gam <- gam(D ~ year, data = df8)
T1 <- visreg(gam,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)


