context('dt2Track')


x = data.frame(pesa56511) %>% data.table
o = dt2Track(x, projection = sp::proj4string(pesa56511) )
a = tdbscan(pesa56511, eps = 6600, minPts = 8, maxLag = 6, borderPoints = TRUE )
b = tdbscan(o, eps = 6600, minPts = 8, maxLag = 6, borderPoints = TRUE )
identical(a, b)

# full function is working
test_that('dt2Track is data.table', {

  expect_s3_class( a, 'data.table' )
  expect_s3_class( b, 'data.table' )

})

