test_that("data_frame should be a dataframe", 
          {expect_error(clean_data(list_wrong_type,required_columns), "data.frame")
          })

test_that("The data frame does not contain all the required columns", 
          {expect_error(clean_data(without_shares_df,required_columns))
            expect_error(clean_data(without_num_hrefs_num_imgs_df,required_columns))
          })

test_that(" data_frame shouldn't be empty", 
          {expect_error(clean_data(empty_df,required_columns))})

test_that(" required_columns shouldn't be empty", 
          {expect_error(clean_data(good_df,required_columns_empty))})

test_that("`clean_data` should return a data frame, or data frame extension, with the required columns",{
  expect_equal(clean_data(good_df,required_columns),check_popular_value)
})
