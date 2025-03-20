#DPS

#03.20.25

library(tidyverse)

data1= data.frame(ID = c(1,2),
                  X1 = c("a1", "a2"))
data1

data2 = data.frame(ID = c(2,3),
                    X2 = c("b1", "b2"))
data2

#Mutating joins
#left is first data set said (read left to right)


data12_left = left_join(data1, data2)
data12_left

##no record of id 3 because we chose left join

data12_left = data1 %>%
  left_join(data2, by="ID")

data12_left


##Right join

data12_right = right_join(data1, data2)
data12_right
dim(data1)
dim(data2)
dim(data12_right)

##inner join
data12_inner = inner_join(data1, data2)
data12_inner


## full join
data12_full = full_join(data1, data2)
data12_full

##Filtering Joins

#Semi Join
data12_semi = data1 %>%
  semi_join(data2)
data12_semi


#Anti join
data12_anti = anti_join(data1, data2, by="ID")
data12_anti


#Exercise 1.1

###Pivots

survey = data.frame(quadrat_id = c(101,102,103,104),
                    barnacle = c(2,11,8,27),
                    chiton = c(1,0,0,2),
                    mussel = c(0,1,1,4))
survey

long = survey%>%
  pivot_longer(cols = c("barnacle", "chiton", "mussel"),
               names_to = "beast",
               values_to = "counts")
long

wide = long %>%
  pivot_wider(names_from = beast,
              values_from = counts)
wide
dim(wide)


##Exercise 1.2

ggplot(data=survey) +
  geom_point(aes(x=counts,
                 y=quadrat_id,
                 fill = "beast"))

ggplot(data=long)+
  geom_point(aes(x=quadrat_id,
                 y=counts,
                 color=beast))
