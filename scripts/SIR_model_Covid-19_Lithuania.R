# https://www.r-bloggers.com/covid-19-the-case-of-germany/
# https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov

library(deSolve)

# https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Germany#Statistics
#Infected <- c(16, 18, 21, 26, 53, 66, 117, 150, 188, 240, 349, 534, 684, 847, 1112, 1460, 1884, 2369, 3062, 3795, 4838, 6012)
Infected <- data_lt_country_cum %>% filter(var == "active")
Infected <- Infected$values
Day <- 1:(length(Infected))
#N <- 83149300 # population of Germany acc. to Destatis
N <- 2830582 # population of Lithuania acc. to Wikipedia (2017)

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
#title("Total infections COVID-19 Germany", outer = TRUE, line = -2)
title("Total infections of COVID-19 in Lithuania", outer = TRUE, line = -2)


SIR <- function(time, state, parameters) {
        par <- as.list(c(state, parameters))
        with(par, {
                dS <- -beta/N * I * S
                dI <- beta/N * I * S - gamma * I
                dR <- gamma * I
                list(c(dS, dI, dR))
        })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
        names(parameters) <- c("beta", "gamma")
        out <- ode(y = init, times = Day, func = SIR, parms = parameters)
        fit <- out[ , 3]
        sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6428120 0.3571881

#t <- 1:80 # time in days
t <- 1:120 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
#title("SIR model Covid-19 Germany", outer = TRUE, line = -2)
title("SIR model Covid-19 Lithuania", outer = TRUE, line = -2)


par(old)

R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0
##       R0 
## 1.799646 DE
## 1.373345 LT

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
##          I
## 54 9769398 DE
## 85 115612.4 LT

max_infected <- max(fit$I)
max_infected / 5 # severe cases
## [1] 1953880 DE
## [1] 23122.47 LT

max_infected * 0.06 # cases with need for intensive care
## [1] 586163.9 DE
## [1] 6936.742 LT

# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
max_infected * 0.007 # deaths with supposed 0.7% fatality rate
## [1] 68385.78 DE
## [1] 809.2866 LT
