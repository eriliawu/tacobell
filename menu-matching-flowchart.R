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
library(magrittr)
library(rsvg)
library(DiagrammeRsvg)
library(DiagrammeR)
library(ggplot2)
library(tidyverse)

### create diagram ----
flow <- "digraph {
graph [layout=dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled]
      
      subgraph cluster_input {
      color='#625a5a'
      style=dashed
      label='Input'
      fontname = 'helvetica-bold'
      node[fillcolor=Beige];
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
      b1[label='Drop non-TB items']
      b2[label='Drop vague items']
      b3[label='Drop non-food items']
      b4[label='Fill out \n abbreviations']
      b5[label='Correct misspellings']
      b6[label='De-dup']
      b0 -> b1 -> b2 -> b3
      b3:e -> b4 
      b4 -> b5 -> b6
      {rank=same; b1; b2; b3;}
      {rank=same; b4; b5; b6;}
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
      label='Manual matching, round 1'
      fontname = 'helvetica-bold'
      node[fillcolor='#FFC300']
      d0[label='5 RAs, training']
      d1[label='100-pair pilot']
      d2[label='Full launch \n - 1:1 match \n - 1000-pair list \n - assign diff list']
      d3[label='Sort pairs']
      d0 -> d1 -> d2 -> d3
      {rank=same; d0; d1; }
      {rank=same; d2; d3;}
      }

      subgraph cluster_yes {
      color='#625a5a'
      style=dashed
      label='Yes list'
      fontname = 'helvetica-bold'
      node[fillcolor='#FF5733']
      e0[label='Both RAs \n rated yes']
      }

      subgraph cluster_maybe {
      color='#625a5a'
      style=dashed
      label='Maybe list'
      fontname = 'helvetica-bold'
      node[fillcolor='#40d8ce']
      f0[label='1 RA rated yes, 1 RA rated no']
      f1[label='3rd RA rates every pair']
      f2[label='Sort pairs']
      f1 -> f2
      {rank=same; f0; f1;}
      }

      subgraph cluster_no {
      color='#625a5a'
      style=dashed
      label='No list'
      fontname = 'helvetica-bold'
      node[fillcolor='#e9a1df']
      g0[label='Filter top 95% \n sales items']
      g1[label='Train RAs']
      g2[label='Find match, \n MenuStat']
      g3[label='Find match, \n internet']
      g4[label='Find proxy, \n MenuStat/internet']
      g5[label='Cannot find match']
      g0 -> g1 -> g2 -> g3 -> g4 -> g5
      {rank=same; g0; g1;}
      {rank=same; g2; g3;}
      {rank=same; g4; g5;}
      }

      subgraph cluster_record {
      color='#625a5a'
      style=dashed
      label='Record nutritional info'
      fontname = 'helvetica-bold'
      node[fillcolor='#add7ec']
      h0[label='Record match \n source']
      h1[label='Record match \n item name']
      h2[label='Record \n nutritional info']
      h0 -> h1 -> h2
      {rank=same; h0; h1;}
      #{rank=same; h2;}
      }

      subgraph cluster_add {
      color='#625a5a'
      style=dashed
      label='Add to database'
      fontname = 'helvetica-bold'
      node[fillcolor='']
      i0[label='Build master \n nutritional table']
      i1[label='Link item names \n to product code']
      i2[label='Add table \n to database']
      i0 -> i1 -> i2
      {rank=same; i0; i1;}
      }

{a0 a1} -> b0
b6 -> c0 -> d0
d3:e -> {e0 f1 g0}
f2 -> g2
{e0 g2 g3 g4} -> h0
h2 -> i0

#ranking
#{rank=same; e0; f0; f1; f2;}

#invisible edges
{a0 a1} -> b1 [style=invis]
b0 -> c0 [style=invis]
b0 -> d0 [style=invis]
d0 -> f1 [style=invis]
h0 -> i0 [style=invis]
h2 -> i1 [style=invis]
f0 -> h0 [style=invis]
d2 -> e0 [style=invis]
c0 -> a1 [style=invis]
b6 -> c0 [style=invis]
}"
grViz(flow) %>%
      export_svg %>% charToRaw %>% rsvg_png("tables/flowchart.png")

### use ggplot2 ----
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
