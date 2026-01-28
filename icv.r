# chi no es simetrica

nconfianzainferior<-function(a,n)
{
  al<-1-a
  dial<-1-al/2
  total<-qchisq(dial,n-1)
  round(total,4)
  
}

nconfianzasuperior<-function(a,n)
{
  al<-1-a
  dial<- al/2
  total<-qchisq(dial,n-1)
  round(total,4)
  
} 
#falta para hallar s2
s<-c(46.4 , 46.1 , 45.8 , 47.0 , 46.1 , 45.9 , 45.2 , 46.0 , 45.8, 46.9)
s1<-var(s)
s1

icv <- function(sapsss, alf,n)
{
  
  xinf<-nconfianzainferior(alf,n)
  xsup <- nconfianzasuperior(alf,n)
  g<- ((n-1)*s1)/xinf
  g2<- ((n-1)*s1)/xsup
  is   <- round(g,3)
  infr <- round(g2,3)
  return(cat(paste("ICP=[", g , g2 , "]")))
  
}
icv(s1,0.95,10)
nconfianzasuperior(0.95,10)
