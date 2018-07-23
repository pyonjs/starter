---
title: "Starter"
author: "James Pyon"
date: "July 22, 2018"
output: html_document
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(newmousedata)
```

```
##  id         obs             eij                yij       
##  5:8   Min.   :1.000   Min.   :-2.58586   Min.   :15.15  
##  3:7   1st Qu.:1.000   1st Qu.:-0.88011   1st Qu.:18.11  
##  2:7   Median :2.000   Median : 0.21445   Median :19.31  
##  1:8   Mean   :2.432   Mean   :-0.02696   Mean   :19.60  
##  4:7   3rd Qu.:3.000   3rd Qu.: 0.77182   3rd Qu.:20.81  
##        Max.   :4.000   Max.   : 3.17174   Max.   :25.76  
##       tx            mousenumber           trial          
##  Length:37          Length:37          Length:37         
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
## 
```

## Including Plots

You can also embed plots, for example:

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
