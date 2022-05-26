#!/usr/bin/env Rscript
B <- 10000
stick <- replicate(B, {
	doors <- as.character(1:3)
  # puts prizes in random order
	prize <- sample(c("car","goat","goat"))    
  # note which door has prize
	prize_door <- doors[prize == "car"]    
  # note which door is chosen
	my_pick  <- sample(doors, 1)   
  # open door with no prize that isn't chosen
	show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    
  # stick with original door
	stick <- my_pick    
  # test whether the original door has the prize
	stick == prize_door    
})
# probability of choosing prize door when sticking
mean(stick)   

switch <- replicate(B, {
	doors <- as.character(1:3)
  # puts prizes in random order
	prize <- sample(c("car","goat","goat"))    
  # note which door has prize
	prize_door <- doors[prize == "car"]    
  # note which door is chosen first
	my_pick  <- sample(doors, 1)    
  # open door with no prize that isn't chosen
	show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    
  # switch to the door that wasn't chosen first or opened
	switch <- doors[!doors%in%c(my_pick, show)]    
  # test whether the switched door has the prize
	switch == prize_door    
})
# probability of switch 
mean(switch)    
