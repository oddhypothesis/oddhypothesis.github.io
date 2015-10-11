---
title: Facing Your Data
layout: post
---

A few years ago, I came across a post on [FlowingData](http://flowingdata.com) 
about [using Chernoff Faces](http://flowingdata.com/2010/08/31/how-to-visualize-data-with-cartoonish-faces/)
as a fun way to visualize multidimensional data:

> The assumption is that we can read people's faces easily in real life, so we 
> should be able to recognize small differences when they represent data. Now 
> that's a pretty big assumption, but debate aside, they're fun to make.

I showed this concept to a coworker, who found it amusing and championed (albeit
in jest) making an application to enable scientists at our company to use faces 
as a standard visualization for data analysis and reporting.  From that point on
it was one  of our running jokes to "face" our data.  Unfortunately, being that
the company was small and everyone (including myself) was always busy, there was 
rarely any spare time to devote to this.  That is, until now ...

I recently accepted a position at UCSD and had a week off between the last day 
at my old job and the first day at my new job.  I thought this would be a good 
time to build a `shiny` application for plotting data with Chernoff Faces.


Chernoff Faces in R
-------------------
To plot Chernoff Faces in R, one uses the `faces()` function from the `aplpack`
package:


```r
library(aplpack)
#> Loading required package: tcltk
faces(mtcars)
```

![](post__oddhypothesis_DFaceR_files/figure-html/unnamed-chunk-2-1.png) 

```
#> effect of variables:
#>  modified item       Var   
#>  "height of face   " "mpg" 
#>  "width of face    " "cyl" 
#>  "structure of face" "disp"
#>  "height of mouth  " "hp"  
#>  "width of mouth   " "drat"
#>  "smiling          " "wt"  
#>  "height of eyes   " "qsec"
#>  "width of eyes    " "vs"  
#>  "height of hair   " "am"  
#>  "width of hair   "  "gear"
#>  "style of hair   "  "carb"
#>  "height of nose  "  "mpg" 
#>  "width of nose   "  "cyl" 
#>  "width of ear    "  "disp"
#>  "height of ear   "  "hp"
```

The side-effects of this function are:

* a plot of faces, each representing individual rows of the data
* a printed `data.matrix` displaying how variables (columns) in the data are
  mapped to facial features.

Aesthetics of the faces aside, they do make it easy to identify similarly
peforming cars in the `mtcars` data set - e.g. Honda Civic, Toyota Corolla, and 
Fiat 128.

There are a couple quirks:

* data needs to be all numeric - any `character` or `factor` columns need to be
  handled (converted) appropriately
* the face drawing algorithm takes a bit of time - I wouldn't recommend it
  for input data with more than 500 observations.  In my opinion, anything more 
  than a 10x10 grid of faces becomes visually overwhelming.


Cleaning your face ... data
---------------------------
Because the data to `faces()` needs to be numeric, here's what happens when 
trying to draw faces using the `iris` data set:


```r
faces(iris)
#> Error in x - min(x): non-numeric argument to binary operator
```

This error occurs because the `Species` column is a factor:


```r
str(iris)
#> 'data.frame':	150 obs. of  5 variables:
#>  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Converting this column to its numeric equivalent successfully creates faces:


```r
set.seed(1234)
sample_rows = sample(1:nrow(iris), 25)

tmp = iris[sample_rows,]
tmp$Species = as.numeric(tmp$Species)
faces(tmp, print.info=F)
```

![](post__oddhypothesis_DFaceR_files/figure-html/unnamed-chunk-5-1.png) 

Alternatively, the `Species` column could be excluded from the data sent to 
`faces()` and used for labelling:


```r
tmp = iris[sample_rows,]
labels = as.character(tmp$Species)
tmp = tmp[-which(colnames(tmp) == 'Species')]
faces(tmp, labels = labels, print.info=F)
```

![](post__oddhypothesis_DFaceR_files/figure-html/unnamed-chunk-6-1.png) 

Considering the above, a couple helper functions are in order.  One to produce
labels from `character` columns:

```r
label_data = function(data) {
  if (is.null(data)) {
    return(NULL)
  }

  col_classes = sapply(data, class)
  cols_char = which(sapply(data, inherits, what='character'))

  labels = NULL
  if (length(cols_char)) {
    if (length(cols_char) > 1) {
      labels = do.call(paste, c(as.list(data[,cols_char]), sep=', '))
    } else {
      labels = data[[cols_char]]
    }
  }

  return(labels)
}
```

and one to remove any `character` columns and convert `factor` columns to numeric
values:

```r
clean_data = function(data) {
  # faces expects a data.matrix-like object with all numeric columns

  if (is.null(data)) {
    return(NULL)
  }

  col_classes = sapply(data, class)
  cols_char = which(sapply(data, inherits, what='character'))
  cols_fctr = which(sapply(data, inherits, what='factor'))

  # try to preserve character columns as labels (row.names)
  if (length(cols_char)) {

    tryCatch({
        row_names = if (length(cols_char) > 1) {
          do.call(paste, c(as.list(data[,cols_char]), sep=', '))
        } else {
          data[[cols_char]]
        }
        rownames(data) = row_names

      },
      error = function(e) {
        # unable to parse rownames, drop completely
        message(sprintf('unable to assign row names: %s', e$message))
      },
      finally = {
        data = data[-cols_char]
      }
    )

  }

  # convert factor columns to integer
  if (length(cols_fctr)) {
    data[,cols_fctr] = sapply(data[,cols_fctr], as.integer)
  }

  return(data)
}
```


Paginated faces
---------------
Plotting  `faces()` for all 150 rows in the `iris` dataset takes nearly three 
seconds on my 5yr old laptop:


```r
system.time({faces(clean_data(iris), print.info=F)})
#>    user  system elapsed 
#>    2.61    0.23    2.84
```

So providing smaller chunks of data to `faces()` will be necessary to keep a
`shiny` application nice and responsive.  Splitting `iris` into multiple 50-row
"pages" is much more snappy:

```r
system.time({
  sample_rows = 1:50
  faces(clean_data(iris)[sample_rows,], print.info=F)
})
#>    user  system elapsed 
#>    0.86    0.11    0.97
```

```r
system.time({
  sample_rows = 51:100
  faces(clean_data(iris)[sample_rows,], print.info=F)
})
#>    user  system elapsed 
#>    0.89    0.08    0.97
```

```r
system.time({
  sample_rows = 101:150
  faces(clean_data(iris)[sample_rows,], print.info=F)
})
#>    user  system elapsed 
#>    0.89    0.07    0.95
```

While `faces()` can perform normalization, it only operates on the data provided.
Paging prior to calling `faces()` requires that the entire data set be normalized
beforehand.  Hence a `scale_data()` function is needed:


```r
scale_data = function(data) {
  # normalizes data to [-1,1] which faces(scale=T) does
  apply(data, 2, function(x) {
    (x - min(x)) / (max(x) - min(x)) * 2 - 1
  })
}
```

Thus the workflow to produce faces for any given page of data is:

```r
data = scale_data(clean_data(raw_data))
page_rows = # ... code to create a list of row indices for pages ... #

# for page_num in 1:length(page_rows) ...
data_page = data[page_rows[[page_num]], ]
face_page = faces(data_page, scale=F, print.info=F, plot.faces=F)
plot(face_page)
```

Shiny faces
-----------
![DFaceR](DFaceR - Google Chrome_2015-10-10_12-46-31.png)

The complete application, DFaceR (pun intended), is published on 
[shinyapps.io](http://oddhypothesis.shinyapps.io/DFaceR).  Source code is 
available on [GitHub](http://github.com/wleepang/DFaceR).

All of the core face plotting functionality was straight forward to build into
a `shiny` application.  The tricky part was building the data paging functionality.

The path of least resistance would have been to use either a `numericInput` or 
`sliderInput` to page through the data.  However, I wanted nice page number and 
prev/next buttons as can be gotten on a `dataTables.js` table.  A quick internet 
search produced nothing that matched my needs.  So, I created my own called which 
I'll describe in more detail in an upcoming post.

For now, enjoy "facing" your data.
