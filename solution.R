library(ggplot2)


## replicate 3.7
PowerCurve <- function(p,m){
  sum <- 0
  for (i in m:100) sum = sum + dbinom(i, 100, p)
  return(sum)
}

x <- seq(.4, 1, .005)
y1 <- PowerCurve(x, 69)
y2 <- PowerCurve(x, 73)

ggplot() + 
  geom_line(mapping = aes(x = x, y = y1)) + 
  geom_line(mapping = aes(x = x, y = y2)) + 
  scale_x_continuous(limits = c(.4, 1), breaks = seq(.4, 1, .1), minor_breaks = NULL) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), minor_breaks = NULL) + 
  xlab("p") + 
  ylab("alpha(p)") + 
  geom_segment(aes(x = 0.6, y = 0.95, xend = 0.8, yend = 0.95)) + 
  geom_segment(aes(x = 0.6, y = 0.05, xend = 0.8, yend = 0.05)) + 
  geom_segment(aes(x = 0.6, y = 0.05, xend = 0.6, yend = 0.95)) + 
  geom_segment(aes(x = 0.8, y = 0.05, xend = 0.8, yend = 0.95))

## explaination
y <- seq(1, 100)
for (i in 1:100) y[i] = PowerCurve(0.6, i)
ggplot(mapping = aes(x = 1:100, y = y)) + 
  geom_line() + 
  scale_x_continuous(limits = c(1, 100), breaks = seq(0, 100, 5)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  geom_abline(intercept = 0.05, slope = 0, col = 'blue') + 
  xlab("m") + 
  ylab("alpha0.6(m)") + 
  labs(title = "The plot of alpha0.6(m) and m")

a = seq(1,6)
for (i in 65:70) a[i-64] = PowerCurve(0.6, i)
df = data.frame(m = 65:70, 'alpha0.6(m)' = a)
df

y <- seq(1, 100)
for (i in 1:100) y[i] = PowerCurve(0.8, i)
ggplot(mapping = aes(x = 1:100, y = y)) + 
  geom_line() + 
  scale_x_continuous(limits = c(1, 100), breaks = seq(0, 100, 5)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  geom_abline(intercept = 0.95, slope = 0, col = 'blue') + 
  xlab("m") + 
  ylab("alpha0.8(m)") + 
  labs(title = "The plot of alpha0.8(m) and m")

a = seq(1,6)
for (i in 70:75) a[i-69] = PowerCurve(0.8, i)
df = data.frame(m = 70:75, 'alpha0.8(m)' = a)
df

