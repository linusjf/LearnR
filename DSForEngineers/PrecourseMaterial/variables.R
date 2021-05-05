#!/usr/bin/env Rscript

B2 <- 7
role.name <- "Scientist"
role_name <- "Scientist"

print(pi)
print(letters)
print(LETTERS)
print(month.name)
print(month.abb)

typeof(1)
typeof("22-02-2001")
is.character("22-02-2001")
is.character(as.Date("22-02-2001"))
as.complex(2)
as.numeric("a")

x <- c(2, 3, 4, 5, 6, 8, 9)
print(x)

ID <- c(1, 2, 3, 4)
emp.name <- c("Man", "Rag", "Sha", "Din")
num.emp <- 4
emp.list <- list(ID, emp.name, num.emp)
print(emp.list)

emp.list <- list(Id = ID, Names = emp.name, `Total staff` = num.emp)
print(emp.list$Names)

print(emp.list[[1]])
print(emp.list[[2]])
print(emp.list[[1]][1])
print(emp.list[[2]][1])

emp.list["Total staff"] <- 5
emp.list[[2]][5] <- "Nir"
emp.list[[1]][5] <- 5
print(emp.list)

emp.ages <- list(ages = c(23, 48, 54, 30, 32))
emp.list <- c(emp.list, emp.ages)
print(emp.list)
