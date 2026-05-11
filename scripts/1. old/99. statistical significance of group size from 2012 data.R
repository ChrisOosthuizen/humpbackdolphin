
g = read.csv("./data/test group size data_2002_2012.csv")
head(g)

t.test(g$X2002, g$X2012, var.equal = T)  

median(g$X2002)
median(g$X2012, na.rm = T)
