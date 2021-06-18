### Dice info ###

dice <- function(num, sides){
  sample(sides, num, TRUE)
}

dice_reroll <- function(num, sides, reroll, times=0){
  rolls <- dice(num, sides)
  if ((length(sides) == 1 && all(1:sides %in% reroll)) ||
      (length(sides) != 1 && all(sides %in% reroll))){
    print('Reroll values cannot be identical to all sides.')
  } else if (times == 0){
    while (any(rolls %in% reroll)){
      rolls[rolls %in% reroll] <- dice(length(rolls[rolls %in% reroll]),sides)
    } 
  } else {
    for(time in times){
      rolls[rolls %in% reroll] <- dice(length(rolls[rolls %in% reroll]),sides)
    }
  }
    return(rolls)
}
du
