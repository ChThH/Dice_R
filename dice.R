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

roll_char_att <- function(num = 4, sides = 6, reroll = 0, drop = 1){
  re <- dice_reroll(num, sides, reroll, times = 0)
  dice_drop <- sort(re)[-(1:drop)]
  return(dice_drop)
}

# written for kid's homework
dicevar <- function(diceface = 6, tol = 0.005, pernum = 5){
  ndice <- diceface
  dicetable <- data.frame (dicerol = 0, ndice = ndice, perc = 0, within = 6)
  while (sum(dicetable$wthin) < diceface) { 
    i <- 1
    while (i <= pernum){
      dicerol <- sample(diceface, ndice, T)
      dicetable <- data.frame(table(dicerol))
      dicetable$ndice <- ndice
      dicetable$perc <- dicetable$Freq / ndice
      dicetable$percexp <- 1/diceface
      dicetable$percdiff <- dicetable$perc - 1/diceface
      dicetable$i <- i
      dicetable$wthin <- dicetable$perc > (1/diceface - tol) & dicetable$perc < (1/diceface + tol)
      # print(dicetable)
      i <- i + 1
      if (sum(dicetable$wthin) != diceface) {break}
    }
    ndice <- ndice + 1
  }
  View(dicetable)
  return(dicetable)
}

