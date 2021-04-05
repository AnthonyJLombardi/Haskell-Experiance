{-
  I was in Class on 8/27/20
  The following function takes a list of ordered pairs and puts the x values
  of the pair in one list and the y values in an other list
-}
myUnZip list
  = ([x | (x,_)<-list], [y| (_,y)<-list])
