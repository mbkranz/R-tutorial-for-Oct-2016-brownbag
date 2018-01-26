#VCHP Brownbag R tutorial Oct. 5th 2016, Mike Kranz
#Using The Tidyverse Packages For Data Analysis
#uses built in dataset provided by ggplot2 (mpg) 
#examples with "fake" datasets for applications to cog psych research
#imported cognitive training time series dataset focusing on longitudinal data 
#(using all parts of data analysis workflow)

#one of my goals was to try to be hipsteR with the tidyverse philosophy of data analysis 
#(at least as much as I can)
#----------> more to come on what this means

#Outline of tutorial#######
#1.) Brief intro to some of the relevant R basics
#2.) R Studio orientation
#3.) Data manipulation/wrangling (dplyr and tidyr packages)
#4.) Data visualization (ggplot2 packages)
#5.) Exploratory Data Modeling and Model Visualization (purrr and broom packages)
#6.) Short example with a cognitive training dataset 

#if you haven't installed the packages on your computer, than install the necessary packages by running code below:
install.packages(c("dplyr","tidyr","broom","purrr","ggplot2","readr"))
#once installed, do not need to do this again, so you can comment them out of code (or delete)

#everytime you open new session of R, import your libraries you plan on using

#library(reshape) #some of the older packages require you to load them before newer packages (e.g., plyr first, then dplyr)
#library(plyr)

library(dplyr) # data manipulation
library(tidyr) # data restructuring
library(broom) #"tidy" modeling (converting stat objects into tidy data frames)
library(purrr) #provides functional programming tools within tidy framework (applies functions over lists)
library(ggplot2) #plotting/visualization
library(readr) #importing data (to "tibble" dataframe) #another package for data import to look at is rio (swiss-army and you just specify import/export)

#NOTE: a new package just came out that installs all tidyverse packages 
#but a few dependencies (modelr and forcats) aren't availabl for newest version of R (3.3.1)--still open issue on github
#but they are not used in this tutorial so we will install relevant packages individually but will work on 3.3.0-->

#install.packages("tidyverse")
#library(tidyverse)


##R basics: some data types etc#######
#will go over other things, like functions, as they come up in tutorial
#specifying variable
test<-"helloworld"

#assign variable and print results
(test<-"helloworld")
#same as
test
#data frames
#fundamental data structure (analogous to datasets in MATLAB): columns of 
helloworld<-data.frame(column1=c("hello","world","test"),
                       column2=c(1,2,3),
                       column3=c(TRUE,FALSE,TRUE),
                       column4=factor(c("hello","world","test")))

#the new tibble data_frame
#refined print method (e.g., shows only first 10 rows and shows class of columns) and 
#can input type (doesn't coerce strings into factors and makes it easier to use with lists). This will be convenient for the tidy data work flow.
helloworld2<-data_frame(columnbad=c("hello","world","test"),
                       column2=c(1,2,3),
                       column3=c(TRUE,FALSE,TRUE),
                       column4=factor(c("hello","world","test")))
options(tibble.width = Inf)

#vectors
helloworld2$column1 #one vector in data frame: with data$vectorname
class(helloworld2$column1)
class(helloworld2$column2)
class(helloworld2$column3)
class(helloworld2$column4)
sapply(helloworld2,class) #won't go over apply family but see https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
#column names
names(helloworld2)
#referring to vectors (columns) in data frame
helloworld2[,c(2,4)] #by position index
helloworld2[,c("columnbad","column4")] #by name
helloworld2$columnbad 
#referring to rows in data frame
helloworld2[c(2,3),]
#logical vector
helloworld2$columnbad=="test"
helloworld2[helloworld2$columnbad=="test" & helloworld2$column3==TRUE,]
#elements within vector
helloworld2[helloworld2$columnbad=="test" & helloworld2$column3==TRUE,]

#lists
hellolist<-list(modelobjects="hello",
                test=c(1,2),
                test2=c("hello","oehl","hell","hdfldf","dfdf","dfdfdfdfdf"))

#now lets look at some actual data with the built-in mpg dataset
#quick descriptive statistics functions for dataset
Hmisc::describe(mpg) #to use functions from packages not loaded (but installed)--> use package::function (useful when there are overlapping function names for different packages and/or you want to remember what package a function came from)
summary(mpg)

modelobject1<-lm(hwy~displ,data=mpg)
modelobject2<-lm(hwy~trans,data=mpg)
modelobject3<-lm(hwy~model,data=mpg)

#lists can also now be incorporated into the new data_frame structure (important for many of the tidyverse tools)
#this makes the data_frame very powerful as a tool to organize data/data analysis pipeline

#mpg is built in dataset from the ggplot2 package (which we will use to learn stuff)
helloworld3<-data_frame(column1=c("hello","world","test"),
                        column2=c(1,2,3),
                        column3=c(TRUE,FALSE,TRUE),
                        column4=factor(c("hello","world","test")),
                        column_data=list(mpg,mpg,mpg),
                        column_modelobjects=list(modelobject1,
                                                 modelobject2,
                                                 modelobject3))

#R Studio orientation#####
#1.) Editor
#2.) Console
#3.) Help
#4.) Environment
#5.) History

##DATA WRANGLING/MANIPULATION#############
#From Dplyr vignette: It provides simple “verbs”, functions that correspond to the most common data manipulation tasks, to help you translate those thoughts into code.
#Thus, provides a language of data manipulation (along with other tidy packages we will talk about)
#The mains verbs include select, filter, mutate, summarise, group_by, and do
#with this language chaining commands together is much easier (we'll see why this is important shortly)
#Less code (well...sometimes): Don't need to use the dataframe$variable to reference variables within dataframe
#Fast: written in C++
#Thus: Easier to read and use
#This is the philosophy of the "tidyverse" set of packages which I try to use/illustrate throughout tutorial

data_mpg<-mpg
#new measures (for demonstration of select tool)
(data_mpg_bad<- mpg %>% rowwise() %>% mutate(bad_drunk_mpg=sample(1:15,1),cell_mpg_bad=sample(1:15,1),sleep_bad_mpg=sample(0:2,1)))

#select(): select a set of columns in data_frame####
select(data_mpg_bad,manufacturer:displ)
select(data_mpg_bad,manufacturer,model,displ)
select(data_mpg_bad,-bad_drunk_mpg,-cell_mpg_bad,-sleep_bad_mpg)
#helper functions that work inside select
#one tool to test out regular expressions is: http://regexr.com/
#starts_with
select(data_mpg_bad,starts_with("bad"))
#ends_with
select(data_mpg_bad,ends_with("bad"))
#matches() matches a regular expression
select(data_mpg_bad,matches("bad$"))
select(data_mpg_bad,matches("^bad"))
select(data_mpg_bad,matches("bad"))
#contains() matches a substring (not regex) #what happens now when we use regular expressions?
select(data_mpg_bad,contains("bad"))
select(data_mpg_bad,contains("^bad"))
select(data_mpg_bad,contains("bad$"))
#one_of() select all variables listed in a character vector (can be useful for custom functions giving you more flexibility)
##select(data_mpg_bad,one_of(c("bad_drunk_mpg","cell_mpg_bad","sleep_bad_mpg","dfdfdff"))) #c('XXX','XXX') is a character vector
data_mpg_bad[,c("bad_drunk_mpg","cell_mpg_bad","sleep_bad_mpg","dfdfdf")]
#we want to get rid of these columns so we will use the matches with a negative sign
select(data_mpg_bad,-matches("bad"))

#everything(): selects all variables not already chosen
select(data_mpg_bad,fl,class,everything()) #can specify variables in addition to helper functions

##mutate(): Add new variables#######
mutate(data_mpg,mpg_avg=(cty+hwy)/2)
#....brief break from mutate to introduce the pipe
#the pipe rearranges code that is more optimal 
#for reading by humans (from left to right rather than inside out)
#integral to the tidyverse workflow/language--> using verbs and now pipes to read left to right

data_mpg_bad %>% select(-matches("bad")) %>% mutate(mpg_avg=(cty+hwy)/2)

#rather than alternative where you must read from inside-out
mutate(select(data_mpg_bad,-matches("bad")),mpg_avg=(cty+hwy)/2)

#and back to mutate

#as you can see from below code with the paste and gsub functions, you can manipulate variables by row with a lot of existing functions 
#with functions suitable to transform an individual element/column

data_mpg2<- data_mpg %>% mutate(trans_category=gsub("\\(.*\\)","",trans), #don't know what this is (just know what auto or manual so I just deleted it)
                               car=paste(manufacturer,model,year,trans,fl,sep="_"), # unique identifier for each row (could also use unite in tidyr...)
                               mpg_avg=rowMeans(.[,c("hwy","cty")]), #class rowMeans (the . means the current dataset)
                               fpg_avg=mpg_avg*5280) #the columns are created in the order that they are specified so you can calc a column from another created column
View(data_mpg2)

#trans_mutate() keeps only variables created 
data_mpg %>% transmute(trans_category=gsub("\\(.*\\)","",trans), #don't know what suffixes are... (just know what auto or manual so I just deleted it)
                                car=paste(manufacturer,model,year,trans,fl,sep="_"), # unique identifier for each row (could also use unite in tidyr...)
                                mpg_avg=rowMeans(.[,c("hwy","cty")]), #class rowMeans (the . means the current dataset)
                                fpg_avg=mpg_avg*5280) #the columns are created in the order that they are specified so you can calc a column from another created column

#you can even do your own custom fxns...

#say you needed to perform two functions to get a new variables
#for example, say you wanted to standardize hwy and cty before getting mean to create a std. composite score
#analogous to if multiple cognitive tasks were on different scales and you wanted a composite score

#specifying a function to use in mutate
#also illustrates using older code (perhaps that you've already become accustomed to using) within the data analysis pipeline
compscoresfxn<-function(tasklist,fdata){rowMeans(scale(fdata[,tasklist]),na.rm = TRUE)}


#reading what this function does from the "inside-out":
#fdata[,tasklist] specifies columns for the dataset
#scale() standardizes these columns
#rowMeans computes mean across these now standardized columns

data_mpg %>% mutate(mpg_avg=compscoresfxn(tasklist=c("cty","hwy"),fdata=.))
#don't need to specify variable names as long as in right order
data_mpg %>% mutate(mpg_avg=compscoresfxn(c("cty","hwy"),.))

#how can we write compscorefxn to go from left to right and take advantage of pipes (for easier reading)?
#example of how pipes can be used more generally (not just for dplyr,tidyr, etc verb functions)
compscoresfxn_tidy<-function(tasklist,fdata){fdata %>% select(one_of(tasklist)) %>% scale() %>% rowMeans(na.rm=TRUE)}
data_mpg %>% mutate(mpg_avg=compscoresfxn_tidy(tasklist=c("cty","hwy"),fdata=.))

#same as above (but without function call)--> can do "sub" pipes as long as it ends with a single vector.
data_mpg %>% mutate(mpg_avg=.[] %>% select(one_of(c("cty","hwy"))) %>% scale() %>% rowMeans(na.rm=TRUE))

#will come back to mutate in a bit during grouped operations...these sub-pipes will also be useful when we get into the nested data structures (lists as vectors in data_frame)

#filter(): return rows with matching conditions (TRUE conditions and not FALSE) #########
#any logical operation that produces boolean values (TRUE/FALSE) across rows can be used
unique(data_mpg2$manufacturer) #manufacturer names in dataset
#or for the sake of being tidy...
data_mpg2 %>% select(manufacturer) %>% unique()

filter(data_mpg2,manufacturer=="ford" | manufacturer=="mercury")
filter(data_mpg2,mpg_avg>30)
#other subsetting besides filter(top_n takes top 2 values of variable specified)
top_n(data_mpg2,2,mpg_avg) 

#we have a lot of car models, but say we are only interested in a few
#first lets look at all the car models 
unique(data_mpg2$model)

#lets only use Mike Kranz's past/current cars
mikecars<-c("maxima","camry","camry solara","caravan 2wd") 
data_mpg2 %>% filter(model %in% mikecars) #basically a whole bunch of or operators (looks for match for each element and outputs TRUE/FALSE)
#equivalent to...
data_mpg2 %>% filter(model=="maxima" | model=="camry" | model=="solara" | model=="caravan 2wd")

#think about if you have a "good subject" list for MRI data or dropped subjects 
#for relational data (combining different datasets--such as demographic information and task data with keys--like subject ids--see join commands))

#can also use the grep family of commands (as they output booleans)
grepl("mid",data_mpg2$class)
data_mpg2 %>% filter(grepl("mid",class))

#arrange(): re-order rows based on variable(s)#########
#may be useful for viewing and also creating variables in some instances
arrange(data_mpg2,mpg_avg)
arrange(data_mpg2,manufacturer,mpg_avg)
arrange(data_mpg2,desc(mpg_avg))

#summarise(): summarise multiple values to a single value#######
data_mpg2 %>% summarise(hwyavg=mean(hwy),ctyavg=mean(cty),totalcars=n())
data_mpg2 %>% summarise(hwysd=sd(hwy),ctysd=sd(cty))

#group_by(): splits dataframe by a variable, performs operations, and then combines output#######
#say you have concatenated task data--> summarise by subject, condition, etc. 
#can do grouped summary or mutate...first lets do summary and then we will come back to a group mutate
data_mpg2 %>% group_by(trans_category) %>% summarise(hwyavg=mean(hwy),ctyavg=mean(cty),mpgavg=mean(mpg_avg))
data_mpg2 %>% 
  group_by(manufacturer) %>% 
  summarise(hwyavg=mean(hwy),ctyavg=mean(cty),mpgavg=mean(mpg_avg),numvehicle=n()) %>%
  arrange(mpgavg)

#tidyr (and reshape) package: tools to restructure data#########

#tidyr is package that "tidies" data (i.e., restructures data) and works well with dplyr
#provides more verbs for the "tidyverse" language/process of data manipulation
#can also use reshape/reshape2--> tidyr is the "newest/hipsteR" way of restructuring your data 

(messy <- data_frame(
  firstname = c("Mike", "Bench", "Role"),
  lastname = c("Kranz", "Warmer", "Player"),
  points = c(22.4, 2.7, 7.1),
  assists = c(10.2, 1.7, 2.3)
))

#gather(): gathers data spread out across multiple columns into one column######
#i.e., goes from "wide" format to "long" format
#must specify the future column name of the your key (column specifying "assist" or "points") 
#and column name of value (the actual value of the assists or points)
#after first two arguments, specify columns to be gathered (unspecified columns will just be duplicated)
(messy_long<-messy %>%
  gather(key=stat,value=avg, points:assists))

#spread(): spread key-value pair across multiple columns#########
#opposite of gather is spread
#putting data into "wide" format (specifying the key and value names already created)
messy_long %>% spread(key=stat,value=avg)

#unite(): unite multiple columns######
#if you want to spread across multiple columns (such as a column for each player with both first and last name, unite these columns)
messy_long %>% unite(col = fullname,firstname:lastname)
#can also use the select helper functions in tidyr functions to specify columns!!!
messy_long %>% unite(col = fullname,contains("name"))
messy_long %>% unite(fullname,firstname:lastname) %>% spread(key = fullname,value= avg)
#other older packages such as reshape do this as well in a different way 
reshape::cast(stat~firstname+lastname,data = messy_long,value="avg")

#separate(): separate one column into many####
#compliment of unite 
messy_long %>% unite(fullname,firstname:lastname)
messy_long %>% unite(fullname,firstname:lastname) %>% separate(col=fullname,into=c("firstname","lastname"),remove = TRUE)

#tidying data_mpg
#let's "gather" the two metrics of miles per gallon leaving the other columns as keys identifying 
#(i.e., unique to) each car
data_mpg %>% gather(key=type,value=mpg,hwy,cty)
#can also use helper functions of select in tidyr fxns
data_mpg %>% gather(mpg,type,one_of("hwy","cty"))

#grouped_by() summarise and mutate and a "fake" cognitive (task switching) dataset#######
#say we have a task switching dataset and we want to compute switch costs for both RT and accuracy

#create the fake cognitive dataset
fakecognitive<- data_frame(Subject=sort(rep(1:10,100)),Trial=rep(1:100,10)) %>% 
  rowwise() %>% 
  mutate(stimuli=sample(c("oddeven","highlow"),1),RT=sample(180:1000,1),accuracy=sample(0:1,1,prob=c(0.2,0.8))) %>% 
  ungroup()
fakecognitive
summary(fakecognitive)

#group_by mutate########
#Repeat or Switch trial? (say we just have what the stimulus was for a particular trial)
fakecognitive_cond<- fakecognitive %>% 
   group_by(Subject) %>% 
   mutate(lasttrial=lag(stimuli),
          condition=ifelse(stimuli!=lag(stimuli),"switch","repeat"),
          goodRT=ifelse(RT>200 & accuracy==1,RT,NA)) %>%
   ungroup() %>%
   filter(Trial!=1)

fakecognitive_cond
#now let's compute a switch cost metric
#lets gather the two measures for each trial: the good RTs and accuracy #other columns will stay be duplicated
fakecognitive_long<- fakecognitive_cond %>% 
  gather(measure,score,accuracy,goodRT)

#group_by summarise########
#now we need to average across trials for each Subject and condition for each measure (ACC and RT)
fakecognitive_long

fakecognitive_avg<-fakecognitive_long %>% 
  group_by(Subject,measure,condition) %>% 
  summarise(avgscore=mean(score,na.rm = TRUE))
#Switch Cost=spread the conditions (switch and repeat) and then compute (mutate) the switch cost 
fakecognitive_avg

fakecognitive_switch<-fakecognitive_avg %>% 
  spread(condition,avgscore) %>%
  mutate(switchcost=switch-`repeat`) #if you want to reverse for accuracy could do ifelse statement

#apparently "repeat" is a reserved word as it is part of the R flow control (like the word "if")
#backquotes allow you to specify that it is a variable name (probably would go back and rename the variables so I don't need to do this)

#One Person Per Row
fakecognitive_switch

fakecognitive_wide<-fakecognitive_switch %>% 
  gather(metricname,avgscore,-Subject,-measure) %>%
  unite(col="metric",metricname,measure) %>%
  spread(metric,avgscore) %>%
  setNames(.,gsub("good","",names(.))) #get rid of the "good" in the name and just use "RT"--> can also do rename() but doesn't allow to replace/delete regular expressions

fakecognitive_wide

#or back to long with our newly computed variables 
#keeping the variable names we just collapsed but also dividing up condition and measure
fakecognitive_wide %>%
  gather(cond_meas,score,-Subject) %>%
  separate(cond_meas,c("condition","measure"),remove=FALSE)

###DATA VISUALIZATION#####
#From R for Data Science by Garrett Grolemund and Hadley Wickham:

#"...R has several systems for making graphs, but ggplot2 is one of the most elegant and most versatile. 
#ggplot2 implements the grammar of graphics, a coherent system for describing and building graphs. 
#With ggplot2, you can do more faster by learning one system and applying it in many places."

#To illustrate ggplot2/tidyverse tools, 
#lets explore if we can predict fuel efficiency based on engine size (displacement), the type of car, 
#and the type of driving done (hwy or cty).

#Basics/template######
ggplot(data = data_mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>)) +

#List of components of ggplot we will go over: 

#	Aesthetics (i.e., <MAPPINGS>)
        #mapping variables to the different aesthetics of a plot
        #like the x/y variables, coloring/differentiating the different bars, points
#	Geoms (i.e., <GEOM_FUNCTION>)
        #functions to add the different plots (like scatterplots, bar plots, violin plots etc)
#	Facets
        #making a grid of plots separated by different variables
              #Plot=[]
              #[cartype1][cartype2][cartype3]
              #[cartype4][cartype5][cartype6]
#	Statistics (part of <GEOM_FUNCTION>) 
        #set of functions often used in combination with geoms
        #e.g., adding a geom with modeling fitting, summarizing,etc

        #here, we will briefly introduce 

#Communication: (making graphs pretty)
        #themes 
            #e.g., changing axes appearance, legend, overall design, etc
        #scales
            #changing how the variables are perceived (like axes labels,ranges, etc)
        #manual aesthetic specification

#Aesthetics######
# Aesthetics (aes): map variables in dataframe onto elements in plot
#	x
#	y
#	colour, linetype,shape, etc
#can also save layers of a plot and use in other plots. 
#can specify aesthetics for subsequent geoms in first ggplot function
ggplot(data = data_mpg2,aes(x = displ, y = hwy, color = class))+geom_point()
#same as...
g1<-ggplot(data = data_mpg2,aes(x = displ, y = hwy, color = class))
g1+geom_point()

#we can get rid of the "data" and "mapping" specifications if in correct order
#not really a good idea to use size for discrete variables
  ggplot(data_mpg) +  #data =
  geom_point(aes(x = displ, y = hwy, size = class)) #mapping = # size #color in blue #alpha #different types 
  
ggplot(data_mpg) +  #data =
  geom_point(aes(x = displ, y = hwy, shape = class)) #mapping = # size #color in blue #alpha #different types 

#what happened here? color "blue" is specified within aesthetic mapping
ggplot(data = data_mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue")) #mapping variable "blue" to a color
#change constant aesthetics outside of aes
ggplot(data = data_mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy),color = "blue",size=5)

#can also specify a different x or y variable in a different geom
ggplot(data = data_mpg2) + 
  geom_point(mapping = aes(x = displ, y = hwy),color="blue")+
  geom_point(mapping = aes(x = displ, y = cty),color="red")

#can also specify different dataframe as well for each geom (example later to come)

#however, given that really our variable is miles per gallon and we want to see two different factors (hwy and city), 
#lets gather these variables and then plot
#notice you can incorporate ggplot into your chain!

data_mpg2 %>% gather(key=type,value=measure,cty,hwy) %>%
  ggplot(aes(x=displ,y=measure,color=type)) + geom_point()

#but let's just make this "long" dataset a variable
#for ease, lets also just call the previous dataset where we have average mpg as data_mpg_wide
data_mpg_long<-data_mpg2 %>% gather(key=type,value=measure,cty,hwy)
data_mpg_wide<-data_mpg2
##facets####
#Let's come back to the class of car...it was a little hard to see 

ggplot(data_mpg_wide) + geom_point(aes(x=displ,y=mpg_avg,color=class))
#hard to read, so lets facet to separate everything out
ggplot(data_mpg_wide) + geom_point(aes(x=displ,y=mpg_avg,color=class))+facet_wrap( ~ class)
ggplot(data_mpg_wide) + geom_point(aes(x=displ,y=mpg_avg,color=class))+facet_wrap( ~ class,nrow=1)

#if we want to facet by 2 variables, lets use facet_grid (variables on vertical and horizontal panels)
#another reason why it may be good 
ggplot(data_mpg_long) + geom_point(aes(x=displ,y=measure,color=class))+facet_grid(class~ type)

#geoms####

#1 variable plots
#geom_histogram (1-D continuous variable)
ggplot(data_mpg_long) + geom_histogram(aes(measure))
ggplot(data_mpg_wide) + geom_histogram(aes(mpg_avg,fill=class))
ggplot(data_mpg_wide) + geom_histogram(aes(mpg_avg,fill=class))+facet_grid(.~class)
#geom_density
ggplot(data_mpg_wide) + 
  geom_density(aes(mpg_avg,fill=class),alpha=.5)

extremes<- c("suv","subcompact")
#notice we can also specify different data in each geom
ggplot(data_mpg_wide) + 
  geom_density(data=data_mpg_wide %>% filter(class %in% extremes),
               aes(mpg_avg,fill=class),alpha=.5)

#specifying different data makes it easy to display different representations of the data.
#as long as you have the same variable in each dataset, you can facet by that variable
#here, we created a one plot with all types of cars and another just with suv and subcompact
ggplot(data_mpg_wide) + 
  geom_density(data=data_mpg_wide %>% 
                 filter(class %in% extremes) %>% 
                 mutate(plottype="extremes"),
               aes(mpg_avg,fill=class),alpha=.5)+
  geom_density(data=data_mpg_wide %>% 
                 mutate(plottype="all"),
               aes(mpg_avg,fill=class),alpha=.5)+
  facet_grid(.~plottype)

#geom_bar (1-D discrete variables--> defaults to frequency count of the variable)
ggplot(data_mpg_wide) + geom_bar(aes(x=class,fill=class))
#error if you try to use with y variable...
ggplot(data_mpg_wide) + geom_bar(aes(x=class,y=hwy,fill=class))
#Error: stat_count() must not be used with a y aesthetic.
#so we need to change the stat used...
ggplot(data_mpg_wide) + geom_bar(aes(x=class,y=hwy,fill=class),stat="summary",fun.y=mean)

#2 variable plots
#geom_point: plots the individual observations (as we did to introduce ggplot above)
#geom_boxplot (X=Discrete,Y=Continous)
ggplot(data_mpg_wide) + geom_boxplot(aes(x=class,y=mpg_avg,fill=class))
#geom_jitter (X=Discrete,Y=Continous)
ggplot(data_mpg_wide) + geom_jitter(aes(x=class,y=mpg_avg,color=class))
#geom_violin (X=Discrete,Y=Continous)
ggplot(data_mpg_wide) + geom_violin(aes(x=class,y=mpg_avg,fill=class))

###adding summary stat layers#####
#all geom functions use stat functions, but you can specify the stats which the geom uses
#for example: perhaps you want to overlay summary stats on top of layer showing distribution
#two way to add summary layers on top of other plots (such as above like violin plot)
#(1) calculate beforehand (such as with dplyr)--> advantage is that you have the values easily stored
#(2) compute with functions in ggplot framework using stats option for geoms

#see the list of stats in help menu for more details on particular stats
#to specify, if you want stat_XXX, then supply stat="XXX"

#1.) adding means#####
#will add means to the violin plot from above
#one way we can do it is to calculate summaries first with dplyr (or other package)
#can add other local data frames to your graph!

(mpg_class_avg<-data_mpg_wide %>% group_by(class) %>% summarise(mpg_mean=mean(mpg_avg)))

ggplot(data_mpg_wide) + geom_violin(aes(x=class,y=mpg_avg,fill=class))+
  geom_point(data=mpg_class_avg,aes(x=class,y=mpg_mean),color="black")

#stats within ggplot instead of calculated outside-->equivalent to previous.
ggplot(data_mpg_wide,aes(x=class,y=mpg_avg)) + geom_violin(aes(fill=class))+
  geom_point(stat="summary",fun.y=mean)

#2.) adding errorbars####
#add error bars to violin plot

#manually calculating both mean and standard error
(mpg_class_avg<-data_mpg_wide %>% group_by(class) %>% summarise(mpg_mean=mean(mpg_avg),
                                                            mpg_se=sd(mpg_avg)/sqrt(n()))) #n() is # of obs in current grouping

ggplot(data_mpg_wide) + geom_violin(aes(x=class,y=mpg_avg,fill=class))+
  geom_point(data=mpg_class_avg,aes(x=class,y=mpg_mean),color="black")+
  geom_errorbar(data=mpg_class_avg,aes(x=class,ymin=mpg_mean-mpg_se,ymax=mpg_mean+mpg_se),size=.5,width=.5)

#within ggplot using stat_summary
ggplot(data_mpg_wide,aes(x=class,y=mpg_avg)) + geom_violin(aes(fill=class))+
  geom_point(stat="summary",fun.y=mean) +
  geom_errorbar(stat="summary",fun.data=mean_se, size=.5,width=.5) #see the list of stats in help menu for more details on particular stats (to specify, if is stat_XXX, then stat="XXX" )

##adding model fits as a layer to your plot(s)########
#within ggplot using geom_smooth
#back to the initial graph hwy (mileage) vs. displ (engine displacement)
ggplot(data_mpg_long) +
  geom_point(aes(x = displ, y =measure,color=type))

#add model fit with geom smooth
#defaults to local regression (i.e,"loess") and with std error regions
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth()

#loess for each type of miles per gallon
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type))

#turning std error off
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type),se=FALSE) 

#other fitting methods and formulas
#linear model
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type),method = "lm")

#second order (quadratic) polynomial--> that's better
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type),method = "lm",formula=y ~ poly(x, 2))

data_mpg_long <- mpg %>% gather(type,measure,cty,hwy)

#both in one plot (with some changes to theme settings)
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type),method = "lm")+
  geom_smooth(aes(color=type),method = "lm",formula=y ~ poly(x, 2))+
  scale_color_manual(guide = guide_legend(title=NULL),
                     values=c("cty"="dodgerblue","hwy"="sienna1"),
                     labels=c("City","Highway"))+
  theme(legend.position="top") #or you could do an c(x,y) vector specifically specifying position
  

#facet above  (imagine if you have many subjects--and want to try and select a good function to model individual growth)
ggplot(data_mpg_long,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_smooth(aes(color=type),method = "lm")+
  geom_smooth(aes(color=type),method = "lm",formula=y ~ poly(x, 2))+facet_wrap(~type)

#fitting models outside and adding to graph rather than with geom_smooth

#by doing the manual way you have more flexibility with quantifying/estimating fit of model stats

#there are a lot of ways to do this--
#such as functions in different modelling packages such as nlme (lmList function) or with plyr tools.
#here I use purrr and broom packages intended to fit functional programming into tidy workflow
#(e.g., operate on multiple  lists within a data frame structure--reminds me of MATLAB datasets/structures)

####model objects into tidy workflow with purrr and broom#####

#takes advantage of the flexiblity of using lists within dataframes to create nested data structures
#ftp://cran.r-project.org/pub/R/web/packages/broom/vignettes/broom.html

#purrr: applies function to a list (nest turns each grouped data into element of list--actually part of tidyr but useful when using purrrr)
#broom: uses tidy, augment, glance as three functions integrated into tidyverse chain of commands with model objects.
#the idea is that model fitting output isn't "tidy" (like predictions and residuals), requiring more code to compare/visualize different models (and put stats into tables for publication)

#to demonstrate this tidy modeling fitting process, 
#lets fit models to the two miles per gallon factors in terms of displ (engine displacement): city and highway and fit the 2 types of models to each
#takes data for each type of mileage (city and highway),and fits separate models to each. 
#Here, we fit the two models we tried above with geom_smooth---
    #a linear model and a second order polynomial...

#1.) make functions to be fitted/mapped then mapped
lmfxn<-function(data){lm(measure~displ,data=data)}
polyfxn<-function(data){lm(measure~poly(displ,2),data=data)}

#2.) nest the group to be fitted (in this case type of miles per gallon)
data_mpg_long %>% group_by(type) %>% nest() 

#3.) map the functions to each data frame in nested data frame 
        #(e.g., which is just simply mapping each element (the data frame of either cty or hwy)---using purrr's "map" function to a list vector.
displ_models<-data_mpg_long %>% group_by(type) %>% nest() %>% 
  mutate(lmfit=map(data,lmfxn),polyfit=map(data,polyfxn))

#this creates a nested data frame structure of the grouped data and model fits
#here we use the purrr function "map" to map the above functions to the data

#ok, so we have our nice tidy nested structure of data and corresponding models. 
displ_models

#Now, lets extract information from these models by mapping (using purrr::map) functions from the package broom.


#first-- just as a reminder about what is in one of these model objects, here is a model object from the R Basics section (we made in the beginning just for example)
    #and some of the stats we can get from a model object
    #really, there is nothing magical about the map or broom functions--> 
    #if something isn't available, it seems like making your own "broom"-like fxns for unsupported packages would be quite easily integrated for use with purrr mappings
modelobject1<-lm(hwy~displ,data=mpg)

#a model object is a list of different pieces of information of model.
#model object list names
names(modelobject1)
#summary of the model fit (a "summary object") which has a specialized print method (not a data frame)
  #we want to make this information "tidy" by putting information into our organized data frame structure
summary(modelobject1)
#to do this, we need to extract information from the different items in the summary object
names(summary(modelobject1))

#tidy-ly extracting model info with broom functions

#tidy: extract parameter statistics for a model in a data_frame
modelobject1 %>% tidy()
#glance: extract summary statistics for a model
modelobject1 %>% glance()
#augment: gives you model predictions/fits for y value observations and stats for each observation--such as residuals)
modelobject1 %>% augment()

#also note that there are a variety of models supported by the broom package 
#(e.g.,nlme and lme4 for mixed models, boot for boostrapping, glmnet for machine learning, I think I saw BUGS as well for Bayesian)

#now lets use map to use broom for each model object in our nested data_frame
#recall our nested data_frame that we created
displ_models

#remember: "map" function applies a function to each element in list
displ_model_stats<-displ_models %>% 
  mutate(glance_lm = lmfit %>% map(broom::glance), #map returns a list or data frame (rather than a vector)
          rsq_lm = glance_lm %>% map_dbl("r.squared"), #map_dbl: input list of data_frames-->for each data frame in list, extract column called "r.squared" of double (numbers)
          tidy_lm = lmfit %>% map(broom::tidy),       
          augment_lm= lmfit %>% map(broom::augment),
          glance_poly = polyfit %>% map(broom::glance),
          rsq_poly = glance_poly %>% map_dbl("r.squared"),
          tidy_poly = polyfit %>% map(broom::tidy),
          augment_poly = polyfit %>% map(broom::augment))

displ_model_graph<-displ_model_stats %>% unnest(data,augment_lm,augment_poly,.sep="")
names(displ_model_graph)<-gsub("augment_|data","",names(displ_model_graph))

#we have our data frame combined with the fitted values--
#know lets make the graph with the same fitted function that geom_smooth did for us
ggplot(displ_model_graph) +
  geom_point(aes(x=displ,y=measure,color=type))+
  geom_line(aes(x=displ,y=lm.fitted,color=type))+
  geom_line(aes(x=displ,y=poly.fitted,color=type))

#and facetted
ggplot(displ_model_graph) +
  geom_point(aes(x=displ,y=measure,color=type))+
  geom_line(aes(x=displ,y=lm.fitted),size=1)+
  geom_line(aes(x=displ,y=poly.fitted),size=1)+facet_grid(.~type)

#lets briefly take a look at what else we have in that displ_model_stats data frame

#remember: we went from nesting our data frame (creating a list of data frames) 
#and  fitting the lm and poly functions to these nested groups
displ_models
#to mapping broom functions (glance,augment,tidy) to each of these elements in the list 
displ_model_stats
#lets extract the list columns of each type of model that we got using the broom fxn "glance"
displ_model_stats %>% unnest(glance_lm) %>% select(r.squared:df.residual)
displ_model_stats %>% unnest(glance_poly) %>% select(r.squared:df.residual)
#now lets see what we have for our parameter estimate in model--beta, std error,etc
displ_model_stats %>% unnest(tidy_lm) 
displ_model_stats %>% unnest(tidy_poly)

###Further communicating visualization results with text layers, themes, scales etc######
##geom_text
#can also put text on graph (such as with our summary statistics)
ggplot(displ_model_graph,aes(x = displ, y = measure)) +
  geom_point(aes(color=type))+
  geom_line(aes(y=lm.fitted,group=type),size=1)+
  geom_line(aes(y=poly.fitted,group=type),size=1)+
  geom_text(data = displ_model_stats,aes(label=paste("lm fit r squared: ",round(rsq_lm,2),sep="")),x=5,y=37)+
  geom_text(data = displ_model_stats,aes(label=paste("lm poly fit r squared: ",round(rsq_poly,2),sep="")),x=5,y=40)+
  facet_grid(.~type)

#scales: adjust various parameters of scale aesthetics such as names, limits, values shown, labels, etc#####
#e.g., x and y axis scales or other aesthetic mapping scales
#see example of how to manipulate scales in cognitive training example at end

#themes: modify various settings for different components of plot #####
#(e.g., axis text, hiding text, legend text/layout/position, background lines etc)
#lets use the violin plot we made earlier to play with the themes
g_violin<-ggplot(data_mpg_wide,aes(x=class,y=mpg_avg)) + geom_violin(aes(fill=class))+
  geom_point(stat="summary",fun.y=mean) +
  geom_errorbar(stat="summary",fun.data=mean_se, size=.5,width=.5) #see the list of stats in help menu for more details on particular stats (to specify, if is stat_XXX, then stat="XXX" )
g_violin
#overall themes (such as black and white)
g_violin+theme_bw()
#legend doesn't really give any new info so lets delete legend
g_violin2<-g_violin+theme_bw()+guides(fill=FALSE) #fill corresponds to aesthetic label (if color, would do color=FALSE)

#specific changes in themes with theme()
g_violin2 +theme(text=element_text(family="Helvetica"),
        panel.grid=element_blank(),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20))

#strip is for facet characteristics
g_violin2 + facet_grid(.~class)
g_violin2 + facet_grid(.~class)+theme(strip.text=element_blank())

####example with a cognitive training dataset for finding a good preliminary multilevel model#####
#change to dir to folder where data is located
setwd("/Users/michaelkranz/Documents/Teaching/Brownbag_Oct5th_RTutorial/RTutorial_VCHP/")
cogtrain_data<-read_csv("cogtrain.csv")
#data manipulation
#need to get into long format as we are dealing with a hierarchical dataset (sessions within subject)
#some people had missing data for session (needed to exclude so fitted values would correspond with actual observations)

#melt sessions into one column, filter out missing values, compute Game and Session columns
(cogtrain_long<-cogtrain_data %>% 
  gather(gamesess,Avglvl,-Subject) %>% 
  filter(!is.na(Avglvl)) %>%
  mutate(Session=as.integer(gsub(".*_","",gamesess)),
         Game=gsub("_.*$","",gamesess)))
#or tidy-y
cogtrain_long<-cogtrain_data %>% 
  gather(gamesess,Avglvl,-Subject) %>% 
  filter(!is.na(Avglvl)) %>%
  separate(gamesess,c("Game","Session"),convert=TRUE,remove=FALSE)

cogtrain_long
#visualization of individual subject training time series data
ggplot(cogtrain_long) + geom_line(aes(x=Session,y=Avglvl))
#it's weird because we need to specify grouping...we want to group by subject
ggplot(cogtrain_long) + geom_line(aes(x=Session,y=Avglvl,group=Subject))
ggplot(cogtrain_long) + geom_line(aes(x=Session,y=Avglvl,group=Subject,color=Subject))
ggplot(cogtrain_long) + geom_point(aes(x=Session,y=Avglvl),size=.5) + facet_wrap(~Subject)

#exploratory model fitting
#purrr: model fits
lmfxn<-function(data){lm(Avglvl~Session,data=data)}
logfxn<-function(data){lm(log(Avglvl)~log(Session),data=data)}
#see  non-linear least squares with the nls command (could do "power law" of practice)

(cogtrain_models<-cogtrain_long %>% 
  group_by(Subject) %>% 
  nest() %>% 
  mutate(lmfit=map(data,lmfxn),logfit=map(data,logfxn)))

#broom and purrr:  model parameter/stats extraction

cogtrain_model_stats<-cogtrain_models %>% 
  mutate(glance_lm = lmfit %>% map(broom::glance),
         rsq_lm = glance_lm %>% map_dbl("r.squared"),
         tidy_lm = lmfit %>% map(broom::tidy),
         augment_lm= lmfit %>% map(broom::augment),
         glance_log = logfit %>% map(broom::glance),
         rsq_log = glance_log %>% map_dbl("r.squared"),
         tidy_log = logfit %>% map(broom::tidy),
         augment_log = logfit %>% map(broom::augment))

#visualization of fitted values

cogtrain_model_graph<-cogtrain_model_stats %>% unnest(data,augment_lm,augment_log,.sep="")
names(cogtrain_model_graph)<-gsub("augment_|data","",names(cogtrain_model_graph))

#make a ggplot object so we don't need to keep typing
g1_cogtrain<-ggplot(cogtrain_model_graph) + geom_point(aes(x=Session,y=Avglvl),size=.5)
#plot fitting values on top of observations
g1_cogtrain+
  geom_line(aes(x=Session,y=lm.fitted),color="blue")+
  geom_line(aes(x=Session,y=log.fitted),color="red")+
  facet_wrap(~Subject)
#fitted values are log transformed log(y). So lets convert them back with the exp() function
g1_cogtrain+
  geom_line(aes(x=Session,y=lm.fitted),color="blue")+
  geom_line(aes(x=Session,y=exp(log.fitted)),color="red")+
  facet_wrap(~Subject)
#hard to see--could filter out subjects and look in window or could view in pdf format to make larger (with export)

#investigating which fxn is a better fit: rsq scatter plot of different models
cogtrain_model_stats #our nested data frame of data,model stats, etc with vectors for rsq values for linear and log transformed models

#using these two vectors, lets create a scatterplot
ggplot(cogtrain_model_stats) + 
  geom_point(aes(x=rsq_lm,y=rsq_log))+
  scale_y_continuous(limits=c(.5,1),name="Average Difficulty")+ #make same axes for both x and y
  scale_x_continuous(limits=c(.5,1),name="Session")+
  geom_abline(aes(intercept=0,slope=1)) #creates diagonal line for visualization











