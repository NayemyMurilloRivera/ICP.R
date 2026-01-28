nconfianza<-function(a)
{
  al<-1-a
  dial<-1-al/2
  total<-qnorm(dial,0,1)
  round(total,3)
  
}


icp <- function(p, alf, n, na)
{
  z <- nconfianza(alf)
  q <- 1 - p
  
  if (na <= 0  )
  {
    o <- sqrt((p*q)/n)
  }
  else
  {
    o <- sqrt((p*q)/n) * sqrt((na - n)/na)
  }
  
  is   <- round((p + (z * o)),3)
  infr <- round((p - (z * o)),3)
  
  return(cat(paste("ICP=[",infr, is, "]")))
  
}
icp(0.85,0.95,40,0)
