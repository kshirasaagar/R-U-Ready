
#Explaining Factors

# Continuous to Categorical - for Univariate/Models
a <- sample(1:50,10)
a
cut(a,3)
cut(a,3,labels = c("Low","Medium","High"))

# Storing Huge Strings in a Compatible Form - for Models
a <- c("Monday","Friday","Saturday","Monday","Sunday","Tuesday","Tuesday","Friday")
a
factor(a)
factor(a, levels = c("Monday","Tuesday","Saturday","Sunday"))
factor(a, levels = c("Monday","Tuesday","Saturday","Sunday"),labels = c("Mon","Tue","Sat","Sun"))
