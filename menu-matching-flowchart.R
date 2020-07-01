### create menu matching flowchart
getwd()
setwd("C:/Users/wue04/OneDrive - NYU Langone Health/tacobell")

current_warning <- getOption("warn")
options(warn = -1)
#options(warn = current_warning)

### install and load packages ----
#install.packages("DiagrammeR")
library(DiagrammeR)
library(ggplot2)
library(tidyverse)

### create diagram ----
DiagrammeR::grViz("digraph {
graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

tb [label = 'Taco Bell \n data', shape = circle, fillcolor = Beige]
ms [label = 'MenuStat Data', shape = circle, fillcolor = Beige]
clean [label = 'Data cleaning']
jc [label = 'Jaccard distance']
match1 [label= 'Manual matching, round 1']
yes [label= 'Yes list', shape = circle, fillcolor = Beige]
maybe [label='Maybe list', shape = circle, fillcolor = Beige]
no [label='No list', shape = circle, fillcolor = Beige]
aggregate [label='Aggregate all nutritional information \n into one table']
add [label='Add table to database']

# edge definitions with the node IDs
{tb ms} -> clean -> jc -> match1 -> {yes maybe -> no} ->aggregate -> add
}")

DiagrammeR::grViz('digraph {
rankdir=LR
graph[bgcolor = "#FDFDFD"]
#edge[style=invis]
node[fontname = "helvetica", width = 1.5, height = 0.5, fontsize=12]

subgraph input {
      node [shape=circle,style=filled, fillcolor = red];
      label = "Input";
      style=dashed;
      color= "#625a5a";
      fontname = "helvetica-bold";
      node [shape=box, style=filled, color=black, fillcolor = "#91cf60"];
      a0[label = "Taco Bell data"]
      a1[label = "MenuStat"]
      a2[style=invis]
      a3[style=invis]
      }
      
subgraph clean {
      node [shape=box,style=filled, fillcolor = red];
      label = "Data cleaning";
      style=dashed;
      color= "#625a5a";
      fontname = "helvetica-bold";
      node [shape=box, style=filled, color=black, fillcolor = "#91cf60"];
      b0[label = "Drop other brands"] #2
      b1[label = "Drop vague items"] #3
      b2[label = "Drop non-food"] #4
      b3[label = "Remove punctuations"] #1
      b4[label = "De-duplicate items"] #7
      b5[label = "Fill out abbreviations"] #5
      b6[label = "Correct misspellings"] #6
      b3 -> b5
      b0 -> b6
      b1 -> b4
      #b2 -> 
      }
      
subgraph jc {
      node [shape=box, style = filled, color=black, fillcolor = "#fee08b"];
      label = "Jaccard distance matching";
      style=dashed;
      color= "#625a5a";
      fontname = "helvetica-bold";
            
      c0[label = "q=1"]
      c1[label = "Multiple \n best matches"]
      c3[style=invis]
      c4[style=invis]
      }
      
subgraph match1 {
      node [shape=box, style = filled, color=black, fillcolor = "#fc8d59"];
      label = "Manual match, round 1";
      style=dashed;
      fontname = "helvetica-bold";
      color="#625a5a"
            
      d0[label = "1:1 matching"]
      d1[label = "100-item pilot"]
      d2[label = "5 RAs"]
      d3[label = "Full launch"];
}

{a0 a1} -> b5 -> c0 -> d0
}')

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
