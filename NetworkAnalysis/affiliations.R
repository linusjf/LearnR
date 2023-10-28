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
personMatrix = classesMatrix %*% t(classesMatrix)
print(personMatrix)
number_of_classes_taken = diag(personMatrix)
diag(personMatrix) <- 0
print(personMatrix)
personNet <- graph.adjacency(personMatrix, mode = "undirected")
plot(personNet, vertex.size = 8, vertex.label.cex = .8, vertex.label.color = "black")
groupMatrix = t(classesMatrix) %*% classesMatrix
# The diagonal details the number of people in each class
print(groupMatrix)
number_of_students <- diag(groupMatrix)
# we again set it to 0
diag(groupMatrix) <- 0
personNet <- graph.adjacency(personMatrix, mode = "undirected")
groupNet <- graph.adjacency(groupMatrix, mode = "undirected")
plot(personNet, vertex.label.cex = .6, vertex.label.color = "black")
plot(groupNet, vertex.size = betweenness(groupNet)/max(betweenness(groupNet)) * 10, vertex.label.cex = .6, vertex.label.color = "black")

# tripartite network
classes_to_departments <- data.frame(class = c("Biostatistics","Islamic Civ", "Calc 1", "Linear Algebra", "Chemistry", "The Modern World-System", "Calc 2", "Social Networks", "Exile and Diaspora"), department = c("Math", "History", "Math", "Math", "Chemistry", "Sociology", "Math", "Sociology", "History"), stringsAsFactors = F)
classes_to_departments_matrix <- table(classes_to_departments)
class(classes_to_departments_matrix) <- "matrix"
people_to_departments <- t(classes_to_departments_matrix) %*% t(classesMatrix)
print(people_to_departments)
people_to_departments_net <- graph.incidence(people_to_departments)
plot(people_to_departments_net, vertex.label.cex = .6, vertex.label.color = "black")
