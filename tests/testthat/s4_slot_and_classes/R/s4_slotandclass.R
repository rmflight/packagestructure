setClass("class1",
         slots = list(slot1 = "matrix"))

setClass("class1_1",
         slots = list(slot1_1 = "class1"))

setClass("class1_2",
         contains = "class1_1",
         slots = list(slot1_2 = "character"))

setClass("class2",
         slots = list(slot1 = "matrix"))

