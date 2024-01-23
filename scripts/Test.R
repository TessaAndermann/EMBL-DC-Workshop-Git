weight_kg <-50
weight_lb <-weight_kg*2.2

my_function <-function(x){
  x<-x+2
}
my_result <-my_function(3)
my_result

animals<- c("mouse", "rat", "dog")
animals
length(animals)
#get the type of data 
class(animals)
#structure of the object
str(animals)
#how to add an element to the beginning of a vector
animals <-c("cincilla", animals)
animals
#No way to break a vector easily
#How to get type
typeof(animals)
#challenge
num_char<- c(1, 2, 3, "a")
typeof(num_char)
#character>number
num_logic <- c(1, 2, 3, TRUE)
num_logic
#numeric >logical
char_logical <-c("a", "b", "c", TRUE)
char_logical
typeof((char_logical))
#character>logical

#Subsetting vectors
animals[2]
#Subsetting 2 elements from vectors
animals[c(1,2)]

more_animals <- animals[c(1,2,3,2,1,4)]
more_animals

weight_g <-c(50, 60, 65, 82)
weight_g[c(FALSE, FALSE, TRUE, TRUE)]
weight_g >63
weight_g[weight_g >63]
weight_g[weight_g >63 & weight_g <80]
#True only if both are true

weight_g[weight_g <58 | weight_g >80]
#Operators are < or > or ==(equal), <== or >=

weight_g ==65
animals =="rat" | animals =="frog"
#%in% helps find all elements corresponding to a vector of elements of our choice
animals %in% c("rat", "frog", "cat")
#get the animals independent of position
animals[animals %in% c("rat", "frog", "cat")]

#Missing data
heights <- c(2,4,4,NA,6)
heights
mean(heights)
#remove na
mean(heights, na.rm =T)
max(heights, na.rm =T)
is.na(heights)
heights[!is.na(heights)]
is.na(heights)
na.omit(heights)
heights[complete.cases(heights)]
#equivalent to is na script above

heights <-c(63, 69, 65, NA, 68, 61)
heights_no_na <-na.omit(heights) #doesn't work
heights_no_na <- heights[!is.na(heights)]
heights_no_na
heights_no_na[heights_no_na >67]
length(heights_no_na[heights_no_na >67])
sum(heights_no_na >67)
#summing turns it into logical and since true is =1 then it will be 2 without having to know length



