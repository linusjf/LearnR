#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : affiliations
# @created     : Saturday Oct 28, 2023 05:28:02 IST
#
# @description :
######################################################################
library(igraph)
library(reshape2)
classes_data <- data.frame(name = c("Leo", "Clement", "Palla", "Filippo"), class1 = c("Biostatistics","Islamic Civ", "Calc 1", "Linear Algebra"), class2 = c("Chemistry", "The Modern World-System", "Calc 2", "Social Networks"), class3 = c("Linear Algebra", "Exile and Diaspora", "Linear Algebra", "The Modern World-System"), stringsAsFactors = FALSE)
classes_data <- melt(classes_data, measure.vars = c("class1", "class2","class3"), value.name = "classes", variable.name = "order")
print(classes_data)
classes_data <- subset(classes_data, select = c("name", "classes"))
print(classes_data)
classesMatrix = table(classes_data)
# And we convert it from a table to a matrix
class(classesMatrix) <- "matrix"
print(classesMatrix)
classesNet <- graph.incidence(classesMatrix, mode = c("all"))
print(classesNet)
plot(classesNet, vertex.label.cex = .6, vertex.label.color = "black")
print(vertex_attr_names(classesNet))
print(vertex_attr(classesNet))
V(classesNet)$shape <- ifelse(V(classesNet)$type == FALSE, "circle", "square")
print(classesNet)
print(vertex_attr(classesNet))
plot(classesNet,
vertex.label.cex = .6,
vertex.label.color = "black")
