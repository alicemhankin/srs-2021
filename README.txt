Descriptions of applications written for Summer Research Scholarship 2021 - Alice Hankin

The first programs (tree, hist, multitype, multitype_2, exptree) are all written in R Shiny. The links to the front-end are included here.
The rest (common_ancestor, common_ancestor_2) are written in base R.

tree
--------------------------------------------------------------------
This program draws a random tree given some geometric offspring mean.
It allows for a parameter change to occur at a given time. Alternatively, the parameter changes when a detection event occurs. Each node has a fixed probability of being detected, independently.
A graph of the population over time is also outputted.

The app can be accessed at: https://alicemh.shinyapps.io/tree/

hist
--------------------------------------------------------------------
This program draws population/time line graphs for many branching processes.
Each line ends either at detection or when the population hits zero (whichever occurs first).
The program also outputs a time histogram, a population histogram and a population/time scatter plot for both the detection events and also the event where the population dies out.

The app can be accessed at: https://alicemh.shinyapps.io/hist/


multitype
--------------------------------------------------------------------
This program describes a multitype branching process described as follows:
 - If a node is "infectious", each time-step it either spreads to a random number of people or it dies
 - If a node is "not infectious", each time-step it stays alive and either remains not infectious or becomes infectious
The user can input the probability a non-infectious node becomes infectious, the probability an infectious node dies, and the mean number of nodes an infectious node can spread to. 

The tree diagram is outputted, as well as a graph of the population over time

The app can be accessed at: https://alicemh.shinyapps.io/multitype/

multitype_2
--------------------------------------------------------------------
This describes the same process as in multitype, however this does not output the tree, only the population over time graph. This allows the user to increase the number of generations the process goes on for.

The app can be accessed at: https://alicemh.shinyapps.io/multitype_2/

exptree
--------------------------------------------------------------------
This program describes a branching process as follows:
 - With some (high) probability, the node remains alive for the next generation
 - Otherwise, it spreads to a random geometrically distributed number of others
The purpose of this is to emulate a continuous time branching process in discrete framework

The program outputs the population over time

The app can be accessed at: https://alicemh.shinyapps.io/exptree/

common_ancestor
--------------------------------------------------------------------
This program outputs a simulated vs theoretical distribution of the most recent common ancestor of a pair of nodes in some generation of a branching process.
The number of trees, samples taken per tree, and generation from which the points are sampled are all hardcoded, but can be easily changed.

common_ancestor_2
--------------------------------------------------------------------
This does the same as common_ancestor, but instead of sampling from multiple trees, it only samples from one tree.
Another (hardcoded) variable is the number of points a tree must have (at minimum) in the order to take a sample.

common_ancestor_3
-------------------------------------------------------------------------------------------------------------------------------------------
This does the same as common_ancestor_2, but draws a partial tree also. This code is a work in progress.
