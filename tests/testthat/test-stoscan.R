context('stoscan')

require(tdbscan)
require(data.table)
require(magrittr)

# z bird
data(zbird)
z = tdbscan(zbird, eps = 12, minPts = 5, maxLag = 5, borderPoints = TRUE )
z = z[, clustID := factor(clustID)]

o = data.frame(zbird) %>% data.table
o = merge(z, o, by.x = 'id', by.y = 'sp.ID')

setnames(o, c('x', 'y'), c('lon', 'lat') )
o = rbindlist(list(copy(o[, tagID := 'bird1']), copy(o[, tagID := 'bird2'])), use.names = TRUE)
o[tagID == 'bird2', lon := lon + 5]
o[!is.na(clustID), ID := paste0(tagID, '_', clustID)]

z = stoscan(o, ID = 'ID', lat = 'lat', lon = 'lon', datetime_ = 'time',
            projection= '+proj=utm +zone=4 +datum=WGS84')


# full function is working
test_that('stoscan is list', {

  expect_type( z,  'list' )

})


















