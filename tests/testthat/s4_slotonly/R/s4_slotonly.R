setClass("class2",
         slots = list(slot1 = "matrix"))

setClass("class1",
         slots = list(slot1 = "class2",
                      slot2 = "character",
                      slot3 = "numeric"))

setClass("class3",
         slots = list(slot1 = "character"))