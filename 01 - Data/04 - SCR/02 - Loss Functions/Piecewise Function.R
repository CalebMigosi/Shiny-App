"Recreate Vectorized Piecewise function"
piecewise <- cmpfun(function(index, input, bd, pw){
  "index = We define the case our boundaries are in
      bd = We define the boundaries themselves
   input = vector of the inputs(2 risk factors)
      pw = power of the specific risk factor"
  ifelse(index == 1,
         #CASE1
         (((max(min(bd[2], input), bd[1]))^pw) -
            ((max(min(bd[2], 0), bd[1]))^pw)),
         ifelse(index == 2,
                #CASE 2
                (((max(bd[1], input))^pw) - 
                   ((max(bd[1], 0)^pw))),    
                ifelse(index == 3,
                       #CASE 3
                       (((min(bd[2], input)^pw) -
                           ((min(bd[2], 0)^pw)))),
                       #CASE 4
                       input^pw
                )
         ))
})