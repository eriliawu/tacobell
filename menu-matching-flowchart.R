### create menu matching flowchart
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
#install.packages("DiagrammeR")
#install.packages("DiagrammeRsvg")
#install.packages("rsvg")
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("tidyverse")
library(magrittr)
library(rsvg)
library(DiagrammeRsvg)
library(DiagrammeR)
library(ggplot2)
library(tidyverse)

### create diagram ----
flow <- "digraph {
graph [layout=dot, rankdir = LR, concentrate=true, splines=ortho]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled]
      
      subgraph cluster_input {
      color='#625a5a'
      style=dashed
      label='Input'
      fontname = 'helvetica-bold'
      node[fillcolor='#00FFFF'];
      a0[label = 'Taco Bell \n data']
      a1[label = 'MenuStat Data']
      }

      subgraph cluster_clean {
      label = 'Data cleaning'
      style=dashed
      color= '#625a5a'
      fontname = 'helvetica-bold'
      node[fillcolor=Linen]
      b0[label='Remove punctuations']
      b1[label='Drop items \n - non-TB items \n - vague items \n - non-food items']
      b2[label='Fill out \n abbreviations']
      b3[label='Correct misspellings']
      b4[label='De-dup \n Taco Bell, N=3,517 \n MenuStat, N=941']
      b0 -> b1 -> b2 -> b3 -> b4
      {rank=same; b0; b1; b2;}
      {rank=same; b3; b4;}
      }

      subgraph cluster_match1 {
      color='#625a5a'
      style=dashed
      label='Matching, computer'
      fontname = 'helvetica-bold'
      node[fillcolor='#DAF7A6']
      c0[label='Jaccard distance \n - q=1 \n - multiple best matches']
      }
      
      subgraph cluster_match2 {
      color='#625a5a'
      style=dashed
      label='Initial manual matching'
      fontname = 'helvetica-bold'
      node[fillcolor='#FFC300']
      d0[label='5 RAs, training']
      d1[label='100-pair pilot']
      d2[label='Full launch \n - 1:1 match \n - 1000-pair list \n - assign diff list \n - kappa=0.716']
      d3[label='Sort pairs']
      d0 -> d1 -> d2 -> d3
      {rank=same; d0; d1; }
      {rank=same; d2; d3;}
      }

      subgraph cluster_yes {
      color='#625a5a'
      style=dashed
      label='Yes list (N=496)'
      fontname = 'helvetica-bold'
      node[fillcolor='#FF5733']
      e0[label='Both RAs \n rated yes']
      }

      subgraph cluster_maybe {
      color='#625a5a'
      style=dashed
      label='Maybe list (N=322)'
      fontname = 'helvetica-bold'
      node[fillcolor='#40d8ce']
      f0[label='1 RA rated yes \n 1 RA rated no']
      f1[label='Filter top 95% \n sales items (n=153)']
      f2[label='Train RA']
      f3[label='3rd RA rates every pair, \n corrects mistake (n=96)']
      f4[label='Sort pairs']
      f1 -> f2 -> f3 -> f4
      {rank=same; f0; f1;}
      {rank=same; f2; f3; f4;}
      }

      subgraph cluster_no {
      color='#625a5a'
      style=dashed
      label='No list (N=2,899)'
      fontname = 'helvetica-bold'
      node[fillcolor='#e9a1df']
      g0[label='Filter top 95% \n sales items \n (n=149)']
      g1[label='Train RAs']
      g2[label='Find match, \n MenuStat \n (n=110)']
      g3[label='Find match, \n internet \n (n=36)']
      g4[label='Find proxy, \n MenuStat/internet \n (n=35)']
      g5[label='Cannot find \n match (n=25)']
      g0 -> g1 -> g2 -> g3 -> g4 -> g5
      {rank=same; g0; g1;}
      {rank=same; g3; g4}
      }

      subgraph cluster_record {
      color='#625a5a'
      style=dashed
      label='Record results'
      fontname = 'helvetica-bold'
      node[fillcolor='#add7ec']
      h0[label='Record match \n source']
      h1[label='Record match \n item name']
      h2[label='Record \n nutritional info']
      h3[label='Build master \n nutritional table']
      h4[label='Link item names \n to product code']
      h5[label='Add table \n to database']
      h0 -> h1 -> h2 
      h2:e -> h3 -> h4 -> h5
      {rank=same; h0; h1; h2;}
      {rank=same; h3; h4; h5;}
      }

{a0 a1} -> b0
b4 -> c0 -> d0
d3 -> {e0 f1 g0}
f4 -> {g2 h0}
{e0 g2 g3 g4} -> h0

#invisible edges
{a0 a1} -> b1 [style=invis]
d2 -> e0 [style=invis]
e0 -> g0 [style=invis]
g4 -> h0 [style=invis]
d2 -> e0 [style=invis]
c0 -> a1 [style=invis]
g3 -> h0 [style=invis]
d3 -> h0 [style=invis]
}"
grViz(flow) %>%
     export_svg %>% charToRaw %>% rsvg_png("tables/product-matching/flowchart.png")

### use ggplot2 ----
#https://rstudio-pubs-static.s3.amazonaws.com/461686_e1fba6bd54864c18a292ac4fc90028ec.html
# create grid and work space
data <- tibble(x= 1:100, y= 1:100)
data %>% 
      ggplot(aes(x, y)) +
      scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
      scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
      theme_linedraw() ->
      start

start +
      geom_rect(xmin = 36, xmax=64, ymin=94, ymax=100, color='black',
                fill='white', size=0.25, size=0.25) +
      ggplot2::annotate('text', x= 50, y=97,label= '446 Patients assessed for eligibility', size=2.5)

### flowchart, taco bell data matching for treatment and comparison groups ----
grViz("digraph {
graph [layout=dot, rankdir = LR, concentrate=true, splines=ortho]
node [shape = rectangle, style = filled]

      subgraph cluster_data {
      color='#625a5a'
      style=dashed
      label='Assemble analytical data'
      fontname = 'helvetica-bold'
      node[fillcolor='#00FFFF'];
      a0[label = 'Aggregate restaurant \n level sales \n to calendar month']
      a1[label='Aggregate nutritional \n characteristics']
      a2[label='Pull in restaurant \n characteristics']
      a3[label='Pull in ACS data']
      a4[label='Identify menu labeling \n implementation time']
      a5[label='Take lagged \n measurements on \n dynamic vars']
      a0 -> a1 -> a2 -> a3 -> a4 -> a5
      {rank=same; a0; a1; a2;}
      {rank=same; a3; a4; a5;}
      }

      subgraph cluster_match {
      label = 'Matching'
      style=dashed
      color= '#625a5a'
      fontname = 'helvetica-bold'
      node[fillcolor=hotpink]
      b0[label='Define lookback \n period, establish \n baseline, t=3']
      b1[label='Isolate one \n cluster of \n treated restaurants']
      b2[label='Reduce data to \n the month of \n labeling implementation']
      b3[label='Select parameters \n -select distance (PS, exact, Mahalanobis, etc) \n -replacement \n -matching ratio \n -caliper \n -subclassification \n -method']
      b4[label='Repeat for \n all clusters']
      b5[label='Combine all \n clusters of \n matched restaurants']
      b0 -> b1 -> b2 -> b3 -> b4 -> b5
      {rank=same; b0; b1; b2;}
      {rank=same; b3; b4; b5; }
      }

      subgraph cluster_matched {
      color='#625a5a'
      style=dashed
      label='Diagnostics'
      fontname = 'helvetica-bold'
      node[fillcolor='#DAF7A6']
      c0[label='Merge back to \n analytical data']
      c1[label='Check covariate \n balance, visualize']
      c2[label='Summary statistics']
      c0 -> c1 -> c2
      {rank=same; c0; c1; c2;}
      }

a5 -> b0
b5 -> c0
a0 -> b0 [style=invis]
a0 -> c0 [style=invis]
b3 -> c0 [style=invis]
}")

grViz(rem) %>%
      export_svg %>% charToRaw %>% rsvg_png("tables/analytic-model/matching-strategy.png")

### complete diagram for menu matching, incl. mturk----
flow <- "digraph {
graph [layout=dot, rankdir = LR, concentrate=true, splines=ortho]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled]
      
      subgraph cluster_input {
      color='#625a5a'
      style=dashed
      label='Input'
      fontname = 'helvetica-bold'
      node[fillcolor='#00FFFF'];
      a0[label = 'Taco Bell \n data']
      a1[label = 'MenuStat Data']
      }

      subgraph cluster_clean {
      label = 'Data cleaning'
      style=dashed
      color= '#625a5a'
      fontname = 'helvetica-bold'
      node[fillcolor=Linen]
      b0[label='Remove punctuations']
      b1[label='Drop items \n - non-TB items \n - vague items \n - non-food items']
      b2[label='Fill out \n abbreviations']
      b3[label='Correct misspellings']
      b4[label='De-dup \n Taco Bell, N=3,517 \n MenuStat, N=941']
      b0 -> b1 -> b2 -> b3 -> b4
      {rank=same; b0; b1; b2;}
      {rank=same; b3; b4;}
      }

      subgraph cluster_match1 {
      color='#625a5a'
      style=dashed
      label='Matching, computer'
      fontname = 'helvetica-bold'
      node[fillcolor='#DAF7A6']
      c0[label='Jaccard distance \n - q=1 \n - multiple best matches']
      }
      
      subgraph cluster_match2 {
      color='#625a5a'
      style=dashed
      label='RA matching'
      fontname = 'helvetica-bold'
      node[fillcolor='#FFC300']
      d0[label='5 RAs, training']
      d1[label='100-pair pilot \n1:1 match \n Kappa=[0.58,0.96]']
      d4[label='100-pair pilot \n1:10 match']
      d2[label='Full launch \n - 1:1 match \n - 1000-pair list \n - assign diff list \n - kappa=0.716']
      d3[label='Sort pairs']
      d0 -> d1 -> d4 -> d2 -> d3
      {rank=same; d0; d1; d4;}
      {rank=same; d2; d3;}
      }

      subgraph cluster_yes {
      color='#625a5a'
      style=dashed
      label='Yes list (N=496)'
      fontname = 'helvetica-bold'
      node[fillcolor='#FF5733']
      e0[label='Both RAs \n rated yes']
      }

      subgraph cluster_maybe {
      color='#625a5a'
      style=dashed
      label='Maybe list (N=322)'
      fontname = 'helvetica-bold'
      node[fillcolor='#40d8ce']
      f0[label='1 RA rated yes \n 1 RA rated no']
      f1[label='Filter top 95% \n sales items (n=153)']
      f2[label='Train RA']
      f3[label='3rd RA rates every pair, \n corrects mistake (n=96)']
      f4[label='Sort pairs']
      f1 -> f2 -> f3 -> f4
      {rank=same; f0; f1;}
      {rank=same; f2; f3; f4;}
      }

      subgraph cluster_no {
      color='#625a5a'
      style=dashed
      label='No list (N=2,899)'
      fontname = 'helvetica-bold'
      node[fillcolor='#e9a1df']
      g0[label='Filter top 95% \n sales items \n (n=149)']
      g1[label='Train RAs']
      g2[label='Find match, \n MenuStat \n (n=110)']
      g3[label='Find match, \n internet \n (n=36)']
      g4[label='Find proxy, \n MenuStat/internet \n (n=35)']
      g5[label='Cannot find \n match (n=25)']
      g0 -> g1 -> g2 -> g3 -> g4 -> g5
      {rank=same; g0; g1;}
      {rank=same; g3; g4}
      }

      subgraph cluster_record {
      color='#625a5a'
      style=dashed
      label='Record results'
      fontname = 'helvetica-bold'
      node[fillcolor='#add7ec']
      h0[label='Record match \n source']
      h1[label='Record match \n item name']
      h2[label='Record \n nutritional info']
      h3[label='Build master \n nutritional table']
      h4[label='Link item names \n to product code']
      h5[label='Add table \n to database']
      h0 -> h1 -> h2 
      h2:e -> h3 -> h4 -> h5
      {rank=same; h0; h1; h2;}
      {rank=same; h3; h4; h5;}
      }
      
      subgraph cluster_mturk {
      color='#625a5a'
      style=dashed
      label='Mturk matching'
      fontname = 'helvetica-bold'
      node[fillcolor='#f7a7a6']
      i0[label='200-pair pilot \n- 1:1 match \n- Yes,maybe,no \n- Rated twice \n -Kappa=0.82']
      i1[label='200-pair pilot \n- 1:5 match \n- Rated 3 times \n- Kappa=[0.505,0.619]']
      {rank=same; i0; i1;}
      }

{a0 a1} -> b0
b4 -> c0 -> d0
d3 -> {e0 f1 g0}
f4 -> {g2 h0}
{e0 g2 g3 g4} -> h0
c0 -> i0
c0 -> i1


#invisible edges
{a0 a1} -> b1 [style=invis]
d2 -> e0 [style=invis]
e0 -> g0 [style=invis]
g4 -> h0 [style=invis]
d2 -> e0 [style=invis]
c0 -> a1 [style=invis]
g3 -> h0 [style=invis]
d3 -> h0 [style=invis]
}"
grViz(flow) %>% export_svg %>% charToRaw %>% rsvg_png("tables/product-matching/flowchart-menu-matching-incl-mturk.png")

