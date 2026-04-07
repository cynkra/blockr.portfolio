test_that("bundled dm loads correctly", {
  rds_path <- system.file("extdata", "portfolio_dm.rds",
    package = "blockr.portfolio", mustWork = TRUE)
  dm_obj <- readRDS(rds_path)

  expect_s3_class(dm_obj, "dm")

  tbls <- dm::dm_get_tables(dm_obj)
  expect_true("metadata" %in% names(tbls))
  expect_true("returns" %in% names(tbls))

  meta <- as.data.frame(tbls[["metadata"]])
  expect_true(all(c("ticker", "name", "asset_class", "region",
    "sub_class") %in% colnames(meta)))
  # 34 ETFs + 2 FX pairs = 36
  expect_equal(nrow(meta), 36L)

  ret <- as.data.frame(tbls[["returns"]])
  expect_true(all(c("date", "ticker", "return") %in% colnames(ret)))
  expect_true(nrow(ret) > 1000)
  expect_true(is.numeric(ret$return))
})

test_that("data block constructor works", {
  blk <- new_portfolio_data_block()
  expect_s3_class(blk, "portfolio_data_block")
  expect_s3_class(blk, "dm_block")
})
