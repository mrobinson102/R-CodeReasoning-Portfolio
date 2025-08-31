# -----------------------------------------------------------------------------
# © 2025 Michelle Goulbourne Robinson. All rights reserved.
# Licensed for non-commercial evaluation only. See LICENSE in the repo root.
# Contact: MichelleGRobinson1@gmail.com for other licensing.
# -----------------------------------------------------------------------------
context('Modular Inverse')
source('../../04_number_theory/modular_inverse_calculator.R')
test_that('inverse exists when gcd(a,m)=1', {
  expect_equal(mod_inverse(3, 11), 4) # 3*4 = 12 ≡ 1 (mod 11)
  expect_null(mod_inverse(6, 9))      # gcd(6,9)!=1
})
