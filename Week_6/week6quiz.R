# week6quiz.R
# [For your convenience], here is the provided code from Jared Lander's R for Everyone,
# 6.7 Extract Data from Web Sites

if (!require(XML)) install.packages('XML')
library(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlPool

# 1. What type of data structure is bowlpool?

#******************Answer
#  data.frame
class(bowlPool)

# 2. Suppose instead you call readHTMLTable() with just the URL argument,
# against the provided URL, as shown below

theURL <- "http://www.w3schools.com/html/html_tables.asp"
hvalues <- readHTMLTable(theURL)
hvalues
class(hvalues)
length(hvalues)

# What is the type of variable returned in hvalues?

#******************Answer
# hvalues is a list which contains the various tables (7) on the website, which is all read into the list

# 3. Write R code that shows how many HTML tables are represented in hvalues

#******************Answer
length(hvalues)

# 4. Modify the readHTMLTable code so that just the table with Number,
# FirstName, LastName, # and Points is returned into a dataframe

#******************Answer
str(hvalues)
# we want the first table

hvalues <- readHTMLTable(theURL, which = 1)
hvalues


# 5. Modify the returned data frame so only the Last Name and Points columns are shown.
#******************Answer
hvalues = hvalues[,-c(1,2)]
hvalues

# 6 Identify another interesting page on the web with HTML table values.
# This may be somewhat tricky, because while
# HTML tables are great for web-page scrapers, many HTML designers now prefer
# creating tables using other methods (such as <div> tags or .png files).

#******************Answer
# List of Vice Presidents of the United States at wikipedia.org
# http://en.wikipedia.org/wiki/List_of_Vice_Presidents_of_the_United_States

# 7 How many HTML tables does that page contain?

#******************Answer
mytable = readHTMLTable("http://en.wikipedia.org/wiki/List_of_Vice_Presidents_of_the_United_States")
length(mytable)
# number of tables = 7


# 8 Identify your web browser, and describe (in one or two sentences)
# how you view HTML page source in your web browser.

#******************Answer
# Google Chrome Version 37.0.2062.124 m
# Right click on the webpage and select 'View page source' - this will bring up a new page that shows the actual HTML
# to only inspect the element of interest, right click and select 'Inspect Element', this opens up just below the webpage and
# you can browser the element of interest and see the HTML code

#
