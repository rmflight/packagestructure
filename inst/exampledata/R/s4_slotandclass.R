setClass("slot_A",
         slots = list(a = "matrix"))

setClass("class_A",
         slots = list(slot_A = "slot_A"))

setClass("class_B",
         contains = "class_A",
         slots = list(slot_B = "character",
                      slot_C = "numeric"))

setClass("class_C",
         contains = "class_A",
         slots = list(slot_D = "matrix",
                      slot_E = "character"))

setClass("class_D",
         contains = "class_B")
