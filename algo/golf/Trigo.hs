x?o=print.sum$map(\i->(-1)^floor(i/2)*x**i/product[1..i])[o,2+o..8+o]
r=readLn
g=do x<-r;x?1;x?0;g
main=r>>g
