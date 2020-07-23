test_that("MNAR_documentation() works", {
  verify_output(
    test_path("test-MNAR_documentation.txt"),
    MNAR_documentation("rank")
  )
})

test_that("document_LSimpute() works", {
  verify_output(
    test_path("test-LSimpute_documentation.txt"),
    document_LSimpute("gene")
  )
})
