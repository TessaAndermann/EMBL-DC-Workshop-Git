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
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

#located in dataraw folder that we created
#we need to read it into R however, that requires tidyverse

library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
install.packages(c("tidyverse", "patchwork", "hexbin", "corrplot", "corrr", "broom", "ggfortify", "BiocManager"))

BiocManager::install("ComplexHeatmap")

dim(surveys) #to extract same as length for table 
nrow(surveys)
ncol(surveys)
head(surveys)
tail(surveys)
names(surveys)#gives column name and is equivalent to colnames(surveys)
rownames(surveys)
summary(surveys) #computes summary stats for each column

#indexing and subsetting
surveys[1,6]
#going into first row and 6th column
#entire first row
surveys[1, ]

surveys[c(1,2,3),c(5,6)]
#equivelent to 
surveys[1:3,5:6]

surveys[(3)]

surveys$sex <-factor(surveys$sex)
#to know the levels
levels(surveys$sex)
nlevels(surveys$sex)
#Force the levels
sex <-factor(c("male","female", "female", "male"))
sex
sex <-factor(sex, levels = c("male", "female")) #to force the different levels as opposite

taxafactor<- factor(surveys$taxa)
genusfactor <- factor(surveys$genus)

#to get the number of each tax and genus
nlevels(surveys$taxa)

#or for number of rabbits
sum(surveys$taxa =="Rabbit")
summary(surveys$taxa)

#how to convert factors, forcing conversion
as.character(sex)

year_fct <- factor(c(1990, 1983, 1977, 1997))
#automatically assigns numerical order
as.numeric(year_fct)
#R created levels 
#fix the years as characters
as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct] #this is the betterway
#[year_fct] contains the numbers corresponding to the levels
as.factor(c(1990, 1983, 1977, 1997))

#renaming factors
plot(surveys$sex)
summary(surveys$sex)
summary(surveys)

sex<-surveys$sex
levels(sex)
#to add NA as a level
sex <-addNA(sex)
levels(sex)
plot(sex)
#change NA to undetermined
levels(sex) #contains character vector, can access levels with indexing
levels(sex)[3] <- "undetermined"
levels(sex)
#you have to plot only factors but not vectors alone

#to change the labels
levels(sex)[1:2] <- c("female", "male")
plot(sex)

#NEXT LECTURE
library(tidyverse)

surveys <-read_csv("data_raw/portal_data_joined.csv")
str(surveys)
#gives each of the columns and summary
summary(surveys)
select(surveys,plot_id, species_id, weight)
#alternatively if tgere are 2 columsnt ath I don't want
select(surveys, -record_id, -species_id)
#to select rows
filter(surveys, year == 1995, sex =="M")

surveys2 <- filter(surveys, weight <5)
surveys_sml <-select(surveys2, species_id, sex, weight)
#Nesting functions gives same resutl
select(filter(surveys, weight <5), species_id, sex, weight)

#%in% lets you check for contents
#%>% is a pipe
surveys %>%
  filter(weight <5) %>%
  select(species_id, sex, weight)

surveys %>%
  filter(year <1995) %>%
  select(year, sex, weight)

#learning mutate
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000, weight_lb = weight_kg*2.2) %>%
  view()

#gives intermediate table with "view"

#Compare two different types of animals and compare weights
surveys %>%
  group_by(sex)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))
#Separate, get mean, combine into new column with new name
#removing NAs from weight only 
surveys %>%
  filter(!is.na(sex)) %>%
  group_by(sex)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))
#With mutate all of the columns are preserved

#Sumarize summarizes the function (mean)

surveys %>%
  filter(!is.na(sex)) %>%
  group_by(sex, species_id)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(sex, species_id)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE)) %>%
  print(n=15)

surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(species_id, sex)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight), max_weight =max(weight)) %>%
  print(n=15)

surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(species_id, sex)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight), max_weight =max(weight)) %>%
  arrange(mean_weight)#orders by this column

#You can arrange by descending as well
surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(species_id, sex)%>%
  summarise(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight), max_weight =max(weight)) %>%
  arrange(desc(mean_weight))#orders by this column

surveys %>%
  count(species, sex) %>%
  arrange(species, desc(n))
#count observations

#how many animals caught
surveys %>%
  count(plot_type)

#mean, min, max hindfoot length
surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarise(mean_length = mean(hindfoot_length, na.rm = TRUE), min_length = min(hindfoot_length), max_length =max(hindfoot_length),
            n =n())
 
#The n here can give the number of observations for each! #IMPORTANT

#heaviest animal
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(species_id) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight), max_weight =max(weight)) %>%
  arrange(desc(mean_weight))
  
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(species) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight), max_weight =max(weight)) %>%
  arrange(desc(mean_weight))

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species_id, weight) %>%
  arrange(year) %>%
  unique()

#Pivoting from long to wide
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight = mean(weight)) %>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill =0)
#values fill helps give a value when the cell is not filled

#going to long format from wider, it separates by columns
surveys_gw %>%
  pivot_longer(names_to = "genus", values_to = "mean-weight", cols = -plot_id)

surveys_long <-
  surveys %>%
  pivot_longer(names_to = "measurement", values_to = "value", cols = c(hindfoot_length,weight)) 
  
 surveys_long %>%
   group_by(year, measurement, plot_type) %>%
   summarize(mean_value =mean(value, na.rm =TRUE)) %>%
   pivot_wider(names_from = measurement, values_from =mean_value)
             
  ### Create a table and then export
 
 surveys_complete <-surveys %>%
   filter(!is.na(weight), 
          !is.na(hindfoot_length),
          !is.na(sex))
   
#write to file
 write_csv(surveys_complete, file = "surveys_complete.csv")
 
 ### Plotting data
plt <-ggplot(
   data =surveys_complete,
   mapping = aes(x=weight,y=hindfoot_length)
 )
plt
str(plt)

plt+geom_point()+
  ggtitle("My first plot")
 
#We use the plus to get the geom plot 
# 1. define ggplot object
# plt <- ggplot(data = <data.frame>, mapping = <aesthetics>)
# x aesthetics
# y aesthetics
# color aesthetics
# shape aesthetics
# ....
# 2. add geometry layer(s)
# geometry functions have "predictable" names
# geom_{point, line, bar, histogram, violin, hex, ...}

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

# let's save that in an object
plt <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt +
  ggtitle("Weight VS hindfoot length")

install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = 'blue') # alpha sets the transparency

# what if I want to use color to map another variable? 
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id)) 

# I can also set the color mapping in the overall ggplot call
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = weight, 
    y = hindfoot_length,
    color = species_id
  )
) +
  geom_point(alpha = 0.1, aes(color = species_id)) 

# Challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight,
    color = plot_type)
) +
  geom_point()
# but geom_point() is not really the best choice here...

ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_boxplot()

# or also, by overlaying boxplots AND points
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_boxplot()+
  geom_jitter(alpha = 0.3, color = "salmon")

# even better
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_jitter(alpha = 0.3, color = "salmon") + 
  geom_boxplot(outlier.shape = NA, fill = NA) # specify transparent boxplots and rm the outliers

# Challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = weight)
) +
  geom_violin() 

# Another challenge
ggplot(
  data = surveys_complete, 
  mapping = aes(
    x = species_id, 
    y = hindfoot_length)
) +
  geom_jitter(alpha = 0.3, aes(color = plot_id)) + 
  geom_boxplot(outlier.shape = NA) 
# plot_id is "seen" as a numeric value, but for us it's a discrete one (i.e. a factor)

# how to define colors:
# named: "red", "green", ...
# rgb values: rgb(red = .3, green = .3, blue = .3)
# hexadecimal code: "dedede"

yearly_count <- surveys_complete %>% 
  count(year, genus)


ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n)) +
  geom_line()
# This doesn't look right

ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         group = genus)) +
  geom_line()
# this looks better!

ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n,
         color = genus)) +
  geom_line()


yearly_count %>% 
  ggplot(mapping = aes(x = year,
                       y = n,
                       color = genus)) +
  geom_line()

yearly_count_graph <- surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year,
                       y = n,
                       color = genus)) +
  geom_line()

yearly_count_graph

# Maybe it's too crowded?
# Let's introduce faceting:
ggplot(data = yearly_count,
       mapping = aes(
         x = year,
         y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

# faceting by 2 variables
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
  )

# if I only want rows
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(genus),
  )

surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_grid(
    cols = vars(genus),
  )

# let's change themes
surveys_complete %>% 
  count(year, genus, sex) %>%     
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw()

plt <- surveys_complete %>%
  count(year, genus, sex) %>%      
  ggplot(mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) + # you can add scales = "free_y" to let every subplot have its own y axis
  theme_bw(base_size = 18)
ggsave(filename = "fig/plot.pdf", 
       plot = plt,
       width = 10,
       height = 10)

# change a lot of aspects of the plot through the theme command
plt + 
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme(legend.position = "bottom", # "none" f you want ot remove it
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# as a general suggestion:
# work incrementally, start from an easy plot 
# and only later adjust every detail
# the plot code can become pretty cumbersome
# but google is your friend!
# Also, think that each additional "layer" allows you to add details
# x and y axis,
# color,
# shape,
# faceting, ...

# Our last super complicated plot
plt <- surveys_complete %>%
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x=year,
      y=n,
      color = sex)) +
  geom_line() +
  facet_wrap(facet= vars(genus),
             scales = "free"
  ) +
  scale_color_manual(values = c("tomato", "dodgerblue"),
                     labels = c("female", "male"),
                     name = "Sex") +
  xlab("Years of observation") +
  ylab("Number of individuals") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", 
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45,
                               hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    strip.background =  element_blank()
  )
plt
















