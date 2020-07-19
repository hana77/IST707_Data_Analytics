library('arules')
install.packages('/Users/hana/Downloads/grid')
library('arulesViz')
#load the data set 
groceries <- read.transactions('/Users/hana/Documents/IST 707 Data Analytics/Week 3 - Assocication Rule Mining/groceries.csv', format="basket", sep=",")
head(groceries)
#item frequency plot for the top 20 items
#type indicates whether item frequencies should be displayed relative or absoulte
#absolute = count
#relative = percentage
itemFrequencyPlot(groceries,topN=20,type="absolute")
itemFrequencyPlot(groceries,topN=20,type="relative")

#Mine rules with the association rule algorithm
#it is required to set the minimum support and confidence values
rules <- apriori(groceries, parameter = list(supp=0.001, conf=0.8))

#show top 5 rules, rounding with 2 digits
options(digits = 2)
inspect(rules[1:5])

#get summary info about all rules
summary(rules)

#sor rules so that we can veiw the most relavant rules first
rules<- sort(rules, by="confidence",decreasing = TRUE)
inspect(rules[1:10])

#generate rules, ex)RHS='whole mile', minlen is to avoid empty left hand side items
rules <- apriori(data=groceries, parameter = list(supp=0.001, conf=0.08,minlen=2),appearance = list(default="lhs",rhs="whole milk"),control = list(verbose=F))
rules <- sort(rules, decreasing = TRUE, by='confidence')
inspect(rules[1:5])
rules <- apriori(data=groceries, parameter = list(supp=0.001, conf=0.15,minlen=2),appearance = list(default="rhs",lhs="whole milk"),control = list(verbose=F))
rules <- sort(rules, decreasing = TRUE, by='confidence')
inspect(rules[1:5])

#Visualize the rules
# http://planspace.org/2013/01/17/fix-r-tcltk-dependency-problem-on-mac
subrules2 <- head(sort(rules, by="lift"),10)
plot(subrules2,method="graph")
plot(subrules2,method="graph",interactive = TRUE)
