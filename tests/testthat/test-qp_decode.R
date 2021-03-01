test_that("Quoted-Printable '=' (=3D) entries don't cause a double decode", {
  expect_equal(qp_decode('=3D40'),
               '=40')
  expect_equal(qp_decode('=3D=40'),
               '=@')
  expect_equal(qp_decode('=3DAA'),
               '=AA')
  expect_true(validUTF8(qp_decode('=3DAA')))
})
