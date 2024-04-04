
test_that("`unzip_URL` should throw error if URL is real", {
  expect_error(unzip_URL(badURL, goodPath))
})

test_that("`unzip_URL` should throw error if attempts to unzip a non-zipped folder", {
  expect_error(unzip_URL(non_zip_URL, goodPath))
})

test_that("Data extracted successfully", {
    unzip_URL(goodURL, goodPath)
    numberOfFiles <- length(list.files(paste('.', 'OnlineNewsPopularity', sep = '/')))
    expect_equal(numberOfFiles,2)
})

test_that("Temp file deleted", {
    unzip_URL(goodURL, goodPath)
    on.exit(unlink('OnlineNewsPopularity', recursive = TRUE))
    expect_false(removeTemp)
})

