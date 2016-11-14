# Coding Club Workshop 1 R Basics 
# Learning how to import and explore data, and make graphs through investigating Edinburgh's biodiversity

# Written by Gergana Daskalova 6/11/16 University of Edinburgh

# Loading the necessary packages
library(dplyr)

# Start with a clean global environment (use the little broom on the right to clean it), so that some left over values from previous analyses don't mess up this one

# Importing Edinburgh Biodiversity Data, publicly available from the NBN Gateway https://data.nbn.org.uk/
# ~ stands for your working directory - if you paste your data file there, you can use this shortcut, otherwise you can write out the file location, e.g. C:/My Documents. Alternatively, you can click the "Import Dataset" button and navigate to wherever you've saved your file.
edidiv <- read.csv("~/edidiv.csv")

# Before we start analysing the data, we will check whether it imported the way we want it to. Check that the column titles are imported as such, sometimes R thinks they are values. Are there any NA values, empty columns? When entering data in Excel, make sure that when you delete values, you right click and select delete, as opposed to just pressing delete on your keyboard - that way the values and the space they occupied are truly gone and there won't be empty columns in R later.
# When choosing column titles, don't use spaces and keep them simple - you will be retyping them a lot in your code, so better not call them something super long.

# It's good practice to always do this - this code displays the first and last data records, as well as a summary of the data
head(edidiv)
tail(edidiv)
summary(edidiv)

# In the summary output, you can check whether your continuous variables are taken as such
# Categorical variables are actual categories
# Sometimes with e.g. years you need to think about whether you want them as a continuous or a categorical variable
# What are the implications of each?

# Now that we have our data, we can determine how many species of each taxon group are found in Edinburgh
# Technically we can do all of this in Excel, but there is no way of tracking what you are copying, pasting and selecting there
# In particular, when dealing with a big dataset, it is so easy to make mistakes when sorting data, and you might not realise it and get stuck analysing wrong data!

# We can create a vector (a series of values) listing how many individual species from each taxa have been recorded in Edinburgh between 2000 and 2016
# To find out the number of different taxa, we will use the function unique() together with the function (length)
# unique() gives us the names of the different taxa, length() counts them for us

length(unique(edidiv$taxonGroup))

# Our vector will have 11 values, one for the species richenss of each taxon group

# Now we need to figure out how many unique species have been recorded for each taxon group
# From the summary output, we can see the overall number of records, but we are looking for species richness, not abundance
summary(edidiv$taxonGroup)

# We will filter out the data for each taxon group and then count the unique species within it
# We will use the package dplyr - here we are only hinting to all the cool stuff we can do with this package - we will learn more about it in later tutorials.

Beetle <- filter(edidiv, taxonGroup=="Beetle")
a <- length(unique(Beetle$taxonName))
Bird <- filter(edidiv, taxonGroup=="Bird")
b <- length(unique(Bird$taxonName))
Butterfly <- filter(edidiv, taxonGroup=="Butterfly")
c <- length(unique(Butterfly$taxonName))
Dragonfly <- filter(edidiv, taxonGroup=="Dragonfly")
d <- length(unique(Dragonfly$taxonName))
Flowering.Plants <- filter(edidiv, taxonGroup=="Flowering.Plants")
e <- length(unique(Flowering.Plants$taxonName))
Fungus <- filter(edidiv, taxonGroup=="Fungus")
f <- length(unique(Fungus$taxonName))
Hymenopteran <- filter(edidiv, taxonGroup=="Hymenopteran")
g <- length(unique(Hymenopteran$taxonName))
Lichen <- filter(edidiv, taxonGroup=="Lichen")
h <- length(unique(Lichen$taxonName))
Liverwort <- filter(edidiv, taxonGroup=="Liverwort")
i <- length(unique(Liverwort$taxonName))
Mammal <- filter(edidiv, taxonGroup=="Mammal")
j <- length(unique(Mammal$taxonName))
Mollusc <- filter(edidiv, taxonGroup=="Mollusc")
k <- length(unique(Mollusc$taxonName))

# R is an object-based language - here we are storing the values for species richness of each taxa in the objects a,b,c,etc.
# Spaces after commas makes things easier to read
# You're probably noticing the pattern in the code above (and lots of copying and pasting!) - there is a much more efficient way we will learn about in the next tutorials!

# We can now combine all those object in one vector and add labels - be careful that the order of values and labels is the same, so that you don't label e.g. fungus diversity as mammal diversity
biodiv <- c(a,b,c,d,e,f,g,h,i,j,k)
names(biodiv) <- c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc")

# We have all the values now, so we can visualise them quickly with the barplot() function
barplot(biodiv)

# There's a few things not quite right - we need to add in axis titles, fix out labels (because they are quite long, some of them are not visible here) and also for plants, n=521, and the y scale only goes up to 500, need to adjust it
# We can use the help() function to figure out what code we can add in to fix the issues, and we also want to save our plot
help(barplot)
help(par)
png("barplot2.png", width=1600, height=600)
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
dev.off()
# The plot has been saved in your working directory; to confirm where that was, you can use getwd() and to change it, you can use setwd()
getwd()

# This was a vector of values, each with a label
# Works alright in this case, since we only have taxa and number of species, but in most cases you will have more values and categories
# For that we will use data frames - once we have made the data frame, we can also save it as a csv file
# What happens if you save the vector as a csv file? Not that informative outside of R!
# Difference between a data frame and a matrix

taxa <- c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc")
taxa_f <- factor(taxa)
richness <- c(a,b,c,d,e,f,g,h,i,j,k)
biodata <- data.frame(taxa_f, richness)
write.csv(biodata, file="biodata.csv")

# Note that if we want to make the same barplot using the data frame, not the vector, we need to slightly change the code
# We currently have a small data frame, but we could have had lots of columns, more variables, etc. - because data frames encompass more information (and more diverse information, not just values), we need to tell the barplot() function exactly what we want it to plot, in our case the richness
barplot(biodata$richness, names.arg=c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc"), xlab="Taxa", ylab="Number of species", ylim=c(0,600))
# We can also zoom into the plot, stretch out the corners to change the dimensions and then we will be able to see all the axis labels
# Saving the zoomed in plot, however, actually saves the small plot, so better to again use the png() function to save the file

# By examining the NBN database for Edinburgh (NT27), we found out how many species from a range of taxa have been recorded in Edinburgh.
