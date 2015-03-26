context("S4 slot and classes")
test_graph_loc <- system.file("testdata", "s4_slot_and_classes.RData", package = "packagestructure")

load(test_graph_loc)

test_that("generate correct graph with slots",
          {
            new_graph <- class_graph("s4_slot_and_classes")
            expect_equal(out_graph, new_graph)
          })

test_that("graph with slots installed",
          {
            library(devtools)
            suppressMessages(devtools::load_all("s4_slot_and_classes"))
            new_graph2 <- class_graph("package:TestS4SlotsAndClasses")
            expect_equal(out_graph, new_graph2)
          })