context("S4 slot only")
test_graph_loc <- system.file("testdata", "s4_slotonly.RData", package = "packagestructure")

load(test_graph_loc)

test_that("generate correct graph with slots",
          {
            new_graph <- class_graph("s4_slotonly")
            expect_equal(out_graph, new_graph)
          })