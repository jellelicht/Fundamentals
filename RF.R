set.seed(1234)
normVec <- function(x) sqrt(sum(x^2)) # TODO Stackoverflow
xSample <- function(p) {
  return(runif(p, -1, 1))
}
generatePoints <- function(p, n) {
  acc = list()
  for(i in 1:n){
    acc[[i]] = xSample(p)
  }
  return(acc)
}
a = 0.8
fn <- function(x) {
  return(exp(-a*normVec(x)))
}
nearestNeighbour <- function(xs) {
  lowestIndex = -1
  lowestMag = Inf
  for(i in 1:length(xs)){
    curr = xs[[i]]
    nv = normVec(curr)
    if(nv < lowestMag){
      lowestMag = nv
      lowestIndex = i
    }
  }
  return(xs[[i]])
  #return(min(sapply(xs, normVec)))
}
deltaY <- function(xs) {
  nn = nearestNeighbour(xs)
  y0 = 1
  yp0 = fn(nn)
  diff = y0 - yp0
  return(diff)
}
nnhelper <- function(xs) {
  nn = nearestNeighbour(xs)
  return(normVec(nn))
}
entry <- function(p) {
  points = generatePoints(p, 1000)
  return(deltaY(points))
}
prettyPlot <- function(ps) {
  ys = sapply(ps, entry)
  plot(ps, ys)
  lo <- loess (ys, ps)
  lines(predict(lo), col="red", lwd=2)
}
wutPlot <- function(xs) {
  twos = sapply(xs, function(x) { return(2^x)})
  plot(xs, sapply(twos, entry))
}
ps = c(1,2,4,8,16,32)
s10 = lapply(ps, function(p) { return(generatePoints(p, 10)) })
s100 = lapply(ps, function(p) { return(generatePoints(p, 100)) })
s1000 = lapply(ps, function(p) { return(generatePoints(p, 1000)) })
#s10000 = lapply(ps, function(p) { return(generatePoints(p, 1)) })

ys1000 <- lapply(s1000, nnhelper)
plot(ps, ys1000, ylab="distance to nn of y(0)", xlab="p", main="Nearest neighbour")
#lines(predict(loess(ys~ps), col='red', lwd=2)

ds1000 <- lapply(s1000, deltaY)
plot(ps, ds1000, ylab="Diff between y(0) and predictor y(0)", xlab="p", main="Prediction error for sample size 1000")






abline(0, 0.05, col=2)


