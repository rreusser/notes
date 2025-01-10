# BLAS
Prefixes:
- `S`: single precision
- `D`: double precision
- `C`: single precision complex
- `Z`: double precision complex

## Level 1
| Netlib | Stdlib | WASM | Description |
| ------ | ------ | ---- | ----------- |
| [SROTG](https://www.netlib.org/lapack/explore-html/d7/dc5/group__rotg_ga55d839ab27a662d848390a2bbe3ee9d3.html#ga55d839ab27a662d848390a2bbe3ee9d3) | [srotg](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/srotg) |  | setup Givens rotation |
| [SROTMG](https://www.netlib.org/lapack/explore-html/d3/dd5/group__rotmg_ga4e2582ebf7f445a7628954183c4cce30.html#ga4e2582ebf7f445a7628954183c4cce30) |  |  | setup modified Givens rotation |
| [SROT](https://www.netlib.org/lapack/explore-html/d1/d45/group__rot_ga432ce08bbcda714c82c7a31552f96937.html#ga432ce08bbcda714c82c7a31552f96937) | [srot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/srot) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/srot-wasm) | apply Givens rotation |
| [SROTM](https://www.netlib.org/lapack/explore-html/dc/d23/group__rotm_ga9b95e7fbcee2aab54d571e3986484808.html#ga9b95e7fbcee2aab54d571e3986484808) | [srotm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/srotm) |  | apply modified Givens rotation |
| [SSWAP](https://www.netlib.org/lapack/explore-html/d7/d51/group__swap_ga9aa6d829e36b30c1f4c0e8f5346defa5.html#ga9aa6d829e36b30c1f4c0e8f5346defa5) | [sswap](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sswap) |  | swap `x` and `y` |
| [SSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_ga3b80044a9dbfcbdcb06e48352ee8d64e.html#ga3b80044a9dbfcbdcb06e48352ee8d64e) | [sscal](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sscal) |  | `x = a*x` |
| [SCOPY](https://www.netlib.org/lapack/explore-html/d5/d2b/group__copy_ga7082b747d31963a7c4bcc9a6e488f6cc.html#ga7082b747d31963a7c4bcc9a6e488f6cc) | [scopy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/scopy) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/scopy-wasm) | copy `x` into `y` |
| [SAXPY](https://www.netlib.org/lapack/explore-html/d5/d4b/group__axpy_gabe0745849954ad2106e633fd2ebfc920.html#gabe0745849954ad2106e633fd2ebfc920) | [saxpy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/saxpy) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/saxpy-wasm) | `y = a*x + y` |
| [SDOT](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_gaa145c21fd5f5b672ac0b4560154682dd.html#gaa145c21fd5f5b672ac0b4560154682dd) | [sdot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sdot) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sdot-wasm) | dot product |
| [SDSDOT](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga5e09e98ca27006a197d7c5fa49a9da4b.html#ga5e09e98ca27006a197d7c5fa49a9da4b) | [sdsdot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sdsdot) |  | dot product with extended precision accumulation |
| [SNRM2](https://www.netlib.org/lapack/explore-html/d1/d2a/group__nrm2_gad179c1611098b5881f147d39afb009b8.html#gad179c1611098b5881f147d39afb009b8) | [snrm2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/snrm2) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/snrm2-wasm) | Euclidean norm |
| [SCNRM2](https://www.netlib.org/lapack/explore-html/d1/d2a/group__nrm2_gaee5779d5d216a7cd8cf83488fb6bb175.html#gaee5779d5d216a7cd8cf83488fb6bb175) | [scnrm2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/scnrm2) |  | Euclidean norm |
| [SASUM](https://www.netlib.org/lapack/explore-html/d5/d72/group__asum_ga5fb1932fad7d47868a711867e4f2c804.html#ga5fb1932fad7d47868a711867e4f2c804) | [sasum](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sasum) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sasum-wasm) | sum of absolute values |
| [ISAMAX](https://www.netlib.org/lapack/explore-html/dd/d52/group__iamax_ga68e936c3ea980ed3916cc159f969b440.html#ga68e936c3ea980ed3916cc159f969b440) | [isamax](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/isamax) |  | index of max abs value |
| [DROTG](https://www.netlib.org/lapack/explore-html/d7/dc5/group__rotg_gaafa91c51f75df6c3f2182032a221c2db.html#gaafa91c51f75df6c3f2182032a221c2db) | [drotg](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/drotg) |  | setup Givens rotation |
| [DROTMG](https://www.netlib.org/lapack/explore-html/d3/dd5/group__rotmg_gaebf62f1c90f0829a0a762a7f8918213f.html#gaebf62f1c90f0829a0a762a7f8918213f) |  |  | setup modified Givens rotation |
| [DROT](https://www.netlib.org/lapack/explore-html/d1/d45/group__rot_gae48ef017306866ac2d5a8c5a52617858.html#gae48ef017306866ac2d5a8c5a52617858) | [drot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/drot) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/drot-wasm) | apply Givens rotation |
| [DROTM](https://www.netlib.org/lapack/explore-html/dc/d23/group__rotm_ga4ffe410ec9e3ba6d5632fd16500c45c2.html#ga4ffe410ec9e3ba6d5632fd16500c45c2) | [drotm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/drotm) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/drotm-wasm) | apply modified Givens rotation |
| [DSWAP](https://www.netlib.org/lapack/explore-html/d7/d51/group__swap_ga780475990528dce288cf4f7bba36c90f.html#ga780475990528dce288cf4f7bba36c90f) | [dswap](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dswap) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dswap-wasm) | swap `x` and `y` |
| [DSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_ga0590f8db475e11dc2c9137bb13887acf.html#ga0590f8db475e11dc2c9137bb13887acf) | [dscal](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dscal) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dscal-wasm) | `x = a*x` |
| [DCOPY](https://www.netlib.org/lapack/explore-html/d5/d2b/group__copy_gafc29c83942509cba5f3764115f0471d5.html#gafc29c83942509cba5f3764115f0471d5) | [dcopy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dcopy) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dcopy-wasm) | copy `x` into `y` |
| [DAXPY](https://www.netlib.org/lapack/explore-html/d5/d4b/group__axpy_gadb136e14634fe4b772a0b034f52f3939.html#gadb136e14634fe4b772a0b034f52f3939) | [daxpy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/daxpy) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/daxpy-wasm) | `y = a*x + y` |
| [DDOT](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga2a42ecc597403b22ad786715c739196b.html#ga2a42ecc597403b22ad786715c739196b) | [ddot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ddot) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ddot-wasm) | dot product |
| [DSDOT](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga17a1bc70455f422325f92943b48c7240.html#ga17a1bc70455f422325f92943b48c7240) | [dsdot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dsdot) |  | dot product with extended precision accumulation |
| [DNRM2](https://www.netlib.org/lapack/explore-html/d1/d2a/group__nrm2_gab5393665c8f0e7d5de9bd1dd2ff0d9d0.html#gab5393665c8f0e7d5de9bd1dd2ff0d9d0) | [dnrm2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dnrm2) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dnrm2-wasm) | Euclidean norm |
| [DZNRM2](https://www.netlib.org/lapack/explore-html/d1/d2a/group__nrm2_ga7f9f9febc6dc1836c9f5e7c1aa00b743.html#ga7f9f9febc6dc1836c9f5e7c1aa00b743) | [dznrm2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dznrm2) |  | Euclidean norm |
| [DASUM](https://www.netlib.org/lapack/explore-html/d5/d72/group__asum_ga829029987b14b622f355aacf54a8e4b9.html#ga829029987b14b622f355aacf54a8e4b9) | [dasum](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dasum) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dasum-wasm) | sum of absolute values |
| [IDAMAX](https://www.netlib.org/lapack/explore-html/dd/d52/group__iamax_gacec03c5109f531c06b4fb301cf1a2d7a.html#gacec03c5109f531c06b4fb301cf1a2d7a) | [idamax](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/idamax) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/idamax-wasm) | index of max abs value |
| [CROTG](https://www.netlib.org/lapack/explore-html/d7/dc5/group__rotg_ga7a6687e52657bef3d876b03391a06d2c.html#ga7a6687e52657bef3d876b03391a06d2c) |  |  | setup Givens rotation |
| [CSROT](https://www.netlib.org/lapack/explore-html/d1/d45/group__rot_gaddabfc13f57f29e3deb1fad5cd76eb25.html#gaddabfc13f57f29e3deb1fad5cd76eb25) | [csrot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/csrot) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/csrot-wasm) | apply Givens rotation |
| [CSWAP](https://www.netlib.org/lapack/explore-html/d7/d51/group__swap_ga1e8d1bbcbd0307e7a3839d0bd10e4118.html#ga1e8d1bbcbd0307e7a3839d0bd10e4118) | [cswap](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/cswap) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/cswap-wasm) | swap `x` and `y` |
| [CSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_gacce468103c83fa18bae078d5f49fefe2.html#gacce468103c83fa18bae078d5f49fefe2) | [cscal](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/cscal) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/cscal-wasm) | `x = a*x` |
| [CSSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_ga38234ecdfde7c9a45753af53d13b0187.html#ga38234ecdfde7c9a45753af53d13b0187) |  |  | `x = a*x` |
| [CCOPY](https://www.netlib.org/lapack/explore-html/d5/d2b/group__copy_gab395a30db1137d3deabe520b8e73097d.html#gab395a30db1137d3deabe520b8e73097d) | [ccopy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ccopy) | [wasm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ccopy-wasm) | copy `x` into `y` |
| [CAXPY](https://www.netlib.org/lapack/explore-html/d5/d4b/group__axpy_ga0b7bac1f4d42514074a48f14f5f9caa0.html#ga0b7bac1f4d42514074a48f14f5f9caa0) | [caxpy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/caxpy) |  | `y = a*x + y` |
| [CDOTU](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga2cce681b6aed3728b893a555b3bee55c.html#ga2cce681b6aed3728b893a555b3bee55c) |  |  | dot product |
| [CDOTC](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga5c189335a4e6130a2206c190579b1571.html#ga5c189335a4e6130a2206c190579b1571) |  |  | dot product, conjugating the first vector |
| [SCASUM](https://www.netlib.org/lapack/explore-html/d5/d72/group__asum_ga89c76eef329f84ba9ed106b34fedab16.html#ga89c76eef329f84ba9ed106b34fedab16) | [scasum](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/scasum) |  | sum of absolute values |
| [ICAMAX](https://www.netlib.org/lapack/explore-html/dd/d52/group__iamax_gafdf273dcc3f020e2aa5c716c1b3d7265.html#gafdf273dcc3f020e2aa5c716c1b3d7265) |  |  | index of max abs value |
| [ZROTG](https://www.netlib.org/lapack/explore-html/d7/dc5/group__rotg_ga97ce56a6808661b36ab62269393ac10e.html#ga97ce56a6808661b36ab62269393ac10e) |  |  | setup Givens rotation |
| [ZDROT](https://www.netlib.org/lapack/explore-html/d1/d45/group__rot_ga8d36fce61400cdbfe43c687206397f44.html#ga8d36fce61400cdbfe43c687206397f44) | [zdrot](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/zdrot) |  | apply Givens rotation |
| [ZSWAP](https://www.netlib.org/lapack/explore-html/d7/d51/group__swap_ga8e324819e4cd92b6fde3ae40c83a5cb3.html#ga8e324819e4cd92b6fde3ae40c83a5cb3) | [zswap](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/zswap) |  | swap `x` and `y` |
| [ZSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_gafac698034497c3148620e35c316d9ffb.html#gafac698034497c3148620e35c316d9ffb) | [zscal](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/zscal) |  | `x = a*x` |
| [ZDSCAL](https://www.netlib.org/lapack/explore-html/d2/de8/group__scal_ga40d50a435a5fcf16cf41fa80d746819f.html#ga40d50a435a5fcf16cf41fa80d746819f) |  |  | `x = a*x` |
| [ZCOPY](https://www.netlib.org/lapack/explore-html/d5/d2b/group__copy_gaca1a115319081adeb0a9b80ec37ce626.html#gaca1a115319081adeb0a9b80ec37ce626) | [zcopy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/zcopy) |  | copy `x` into `y` |
| [ZAXPY](https://www.netlib.org/lapack/explore-html/d5/d4b/group__axpy_gaf603daa00d5c723d0e409d9b2d011bf4.html#gaf603daa00d5c723d0e409d9b2d011bf4) | [zaxpy](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/zaxpy) |  | `y = a*x + y` |
| [ZDOTU](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga6b0b69474b384d45fc4c7b1f7ec5959f.html#ga6b0b69474b384d45fc4c7b1f7ec5959f) |  |  | dot product |
| [ZDOTC](https://www.netlib.org/lapack/explore-html/d1/dcc/group__dot_ga2d59b29ec40fb1bedeb3f10205155ee6.html#ga2d59b29ec40fb1bedeb3f10205155ee6) |  |  | dot product, conjugating the first vector |
| [DZASUM](https://www.netlib.org/lapack/explore-html/d5/d72/group__asum_gaf23444d1c822b34db864558d7afc76dd.html#gaf23444d1c822b34db864558d7afc76dd) |  |  | sum of absolute values |
| [IZAMAX](https://www.netlib.org/lapack/explore-html/dd/d52/group__iamax_gafef3b8a5f619fccb1f5208f26e084457.html#gafef3b8a5f619fccb1f5208f26e084457) |  |  | index of max abs value |


## Level 2
| Netlib | Stdlib | WASM | Description |
| ------ | ------ | ---- | ----------- |
| [SGEMV](https://www.netlib.org/lapack/explore-html/d7/dda/group__gemv_ga0d35d880b663ad18204bb23bd186e380.html#ga0d35d880b663ad18204bb23bd186e380) | [sgemv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sgemv) |  | matrix vector multiply |
| [SGBMV]() |  |  | banded matrix vector multiply |
| [SSYMV](https://www.netlib.org/lapack/explore-html/db/d17/group__hemv_ga8990fe737209f3401522103c85016d27.html#ga8990fe737209f3401522103c85016d27) | [ssymv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ssymv) |  | symmetric matrix vector multiply |
| [SSBMV]() |  |  | symmetric banded matrix vector multiply |
| [SSPMV](https://www.netlib.org/lapack/explore-html/d0/d4b/group__hpmv_gacdad62873d30076fb56e99100e8a8a6c.html#gacdad62873d30076fb56e99100e8a8a6c) | [sspmv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sspmv) |  | symmetric packed matrix vector multiply |
| [STRMV](https://www.netlib.org/lapack/explore-html/d6/d1c/group__trmv_ga7b90369d2b2b19f78f168e10dd9eb8ad.html#ga7b90369d2b2b19f78f168e10dd9eb8ad) | [strmv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/strmv) |  | triangular matrix vector multiply |
| [STBMV]() |  |  | triangular banded matrix vector multiply |
| [STPMV]() |  |  | triangular packed matrix vector multiply |
| [STRSV](https://www.netlib.org/lapack/explore-html/dd/dc3/group__trsv_ga9a68aa7057b7b8b6e1eb404144a7b6a1.html#ga9a68aa7057b7b8b6e1eb404144a7b6a1) | [strsv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/strsv) |  | solving triangular matrix problems |
| [STBSV]() |  |  | solving triangular banded matrix problems |
| [STPSV]() |  |  | solving triangular packed matrix problems |
| [SGER]() |  |  | performs the rank 1 operation `A := alpha*x*y' + A` |
| [SSYR](https://www.netlib.org/lapack/explore-html/dc/d82/group__her_gad7585662770cdd3001ed08c7a864cd21.html#gad7585662770cdd3001ed08c7a864cd21) | [ssyr](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ssyr) |  | performs the symmetric rank 1 operation `A := alpha*x*x' + A` |
| [SSPR]() | [sspr](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sspr) |  | symmetric packed rank 1 operation  `A := alpha*x*x' + A` |
| [SSYR2]() | [ssyr2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/ssyr2) |  | performs the symmetric rank 2 operation, `A := alpha*x*y' + alpha*y*x' + A` |
| [SSPR2]() |  |  | performs the symmetric packed rank 2 operation, `A := alpha*x*y' + alpha*y*x' + A` |
| [DGEMV](https://www.netlib.org/lapack/explore-html/d7/dda/group__gemv_ga4ac1b675072d18f902db8a310784d802.html#ga4ac1b675072d18f902db8a310784d802) | [dgemv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dgemv) |  | matrix vector multiply |
| [DGBMV]() |  |  | banded matrix vector multiply |
| [DSYMV](https://www.netlib.org/lapack/explore-html/db/d17/group__hemv_ga0b20bcf6e94079dce2f3d035798e9738.html#ga0b20bcf6e94079dce2f3d035798e9738) | [dsymv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dsymv) |  | symmetric matrix vector multiply |
| [DSBMV]() |  |  | symmetric banded matrix vector multiply |
| [DSPMV](https://www.netlib.org/lapack/explore-html/d0/d4b/group__hpmv_ga739f8dc2316523832bde2b237fcad8a6.html#ga739f8dc2316523832bde2b237fcad8a6) | [dspmv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dspmv) |  | symmetric packed matrix vector multiply |
| [DTRMV](https://www.netlib.org/lapack/explore-html/d6/d1c/group__trmv_ga73370bd6dca01abe05d54ecd1d91ce9a.html#ga73370bd6dca01abe05d54ecd1d91ce9a) | [dtrmv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dtrmv) |  | triangular matrix vector multiply |
| [DTBMV]() |  |  | triangular banded matrix vector multiply |
| [DTPMV]() |  |  | triangular packed matrix vector multiply |
| [DTRSV](https://www.netlib.org/lapack/explore-html/dd/dc3/group__trsv_ga7a7dcbb8745b4776ce13063ab031141f.html#ga7a7dcbb8745b4776ce13063ab031141f) | [dtrsv](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dtrsv) |  | solving triangular matrix problems |
| [DTBSV]() |  |  | solving triangular banded matrix problems |
| [DTPSV]() |  |  | solving triangular packed matrix problems |
| [DGER]() | [dger](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dger) |  | performs the rank 1 operation `A := alpha*x*y' + A` |
| [DSYR](https://www.netlib.org/lapack/explore-html/dc/d82/group__her_ga07f0e3f8592107877f12a554a41c7413.html#ga07f0e3f8592107877f12a554a41c7413) | [dsyr](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dsyr) |  | performs the symmetric rank 1 operation `A := alpha*x*x' + A` |
| [DSPR]() | [dspr](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dspr) |  | symmetric packed rank 1 operation `A := alpha*x*x' + A` |
| [DSYR2](https://www.netlib.org/lapack/explore-html/dd/de5/group__her2_ga8e576e9319c25b883b11dc1f39366bcc.html#ga8e576e9319c25b883b11dc1f39366bcc) | [dsyr2](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dsyr2) |  | performs the symmetric rank 2 operation, `A := alpha*x*y' + alpha*y*x' + A` |
| [DSPR2]() |  |  | performs the symmetric packed rank 2 operation, `A := alpha*x*y' + alpha*y*x' + A` |
| [CGEMV](https://www.netlib.org/lapack/explore-html/d7/dda/group__gemv_ga44c85a0d7ecd60a6bc8ca27b222d7792.html#ga44c85a0d7ecd60a6bc8ca27b222d7792) |  |  | matrix vector multiply |
| [CGBMV]() |  |  | banded matrix vector multiply |
| [CHEMV](https://www.netlib.org/lapack/explore-html/db/d17/group__hemv_ga2308ede4c4300ca4cfac83f2531aec22.html#ga2308ede4c4300ca4cfac83f2531aec22) |  |  | hermitian matrix vector multiply |
| [CHBMV]() |  |  | hermitian banded matrix vector multiply |
| [CHPMV](https://www.netlib.org/lapack/explore-html/d0/d4b/group__hpmv_ga8615e798761db472ab0abb8f67594287.html#ga8615e798761db472ab0abb8f67594287) |  |  | hermitian packed matrix vector multiply |
| [CTRMV](https://www.netlib.org/lapack/explore-html/d6/d1c/group__trmv_ga0adaf80ae1dfe117390bd7030fd865f1.html#ga0adaf80ae1dfe117390bd7030fd865f1) |  |  | triangular matrix vector multiply |
| [CTBMV]() |  |  | triangular banded matrix vector multiply |
| [CTPMV]() |  |  | triangular packed matrix vector multiply |
| [CTRSV](https://www.netlib.org/lapack/explore-html/dd/dc3/group__trsv_gab8c2d2a6476f67197ba1f92aff4a0b92.html#gab8c2d2a6476f67197ba1f92aff4a0b92) |  |  | solving triangular matrix problems |
| [CTBSV]() |  |  | solving triangular banded matrix problems |
| [CTPSV]() |  |  | solving triangular packed matrix problems |
| [CGERU]() |  |  | performs the rank 1 operation `A := alpha*x*y' + A` |
| [CGERC]() |  |  | performs the rank 1 operation `A := alpha*x*conjg( y' ) + A` |
| [CHER](https://www.netlib.org/lapack/explore-html/dc/d82/group__her_gae92bde9f7f6f83c0edaa96f4f2bb2bc3.html#gae92bde9f7f6f83c0edaa96f4f2bb2bc3) |  |  | hermitian rank 1 operation `A := alpha*x*conjg(x') + A` |
| [CHPR]() |  |  | hermitian packed rank 1 operation `A := alpha*x*conjg( x' ) + A` |
| [CHER2](https://www.netlib.org/lapack/explore-html/dd/de5/group__her2_gaf421f493e6422d08e3acfc6d33bfbf46.html#gaf421f493e6422d08e3acfc6d33bfbf46) |  |  | hermitian rank 2 operation |
| [CHPR2]() |  |  | hermitian packed rank 2 operation |
| [ZGEMV](https://www.netlib.org/lapack/explore-html/d7/dda/group__gemv_ga89e23232eb8a7297e5f15f2b404f6ab9.html#ga89e23232eb8a7297e5f15f2b404f6ab9) |  |  | matrix vector multiply |
| [ZGBMV]() |  |  | banded matrix vector multiply |
| [ZHEMV](https://www.netlib.org/lapack/explore-html/db/d17/group__hemv_ga8e55e480b23945c09c35e09935fce058.html#ga8e55e480b23945c09c35e09935fce058) |  |  | hermitian matrix vector multiply |
| [ZHBMV]() |  |  | hermitian banded matrix vector multiply |
| [ZHPMV](https://www.netlib.org/lapack/explore-html/d0/d4b/group__hpmv_gacb6812cc95b64a60f69179b1ca50ead8.html#gacb6812cc95b64a60f69179b1ca50ead8) |  |  | hermitian packed matrix vector multiply |
| [ZTRMV](https://www.netlib.org/lapack/explore-html/d6/d1c/group__trmv_ga1ab0d018131d258a1a3ea755bde73d45.html#ga1ab0d018131d258a1a3ea755bde73d45) |  |  | triangular matrix vector multiply |
| [ZTBMV]() |  |  | triangular banded matrix vector multiply |
| [ZTPMV]() |  |  | triangular packed matrix vector multiply |
| [ZTRSV](https://www.netlib.org/lapack/explore-html/dd/dc3/group__trsv_gad76661a1a371429ec99d8b17c38251bc.html#gad76661a1a371429ec99d8b17c38251bc) |  |  | solving triangular matrix problems |
| [ZTBSV]() |  |  | solving triangular banded matrix problems |
| [ZTPSV]() |  |  | solving triangular packed matrix problems |
| [ZGERU]() |  |  | performs the rank 1 operation `A := alpha*x*y' + A` |
| [ZGERC]() |  |  | performs the rank 1 operation `A := alpha*x*conjg( y' ) + A` |
| [ZHER](https://www.netlib.org/lapack/explore-html/dc/d82/group__her_gaf848c3e119d49365476615e0850bbe5b.html#gaf848c3e119d49365476615e0850bbe5b) |  |  | hermitian rank 1 operation `A := alpha*x*conjg(x') + A` |
| [ZHPR]() |  |  | hermitian packed rank 1 operation `A := alpha*x*conjg( x' ) + A` |
| [ZHER2](https://www.netlib.org/lapack/explore-html/dd/de5/group__her2_ga565bc80dabd392f5a1cb900768b21db4.html#ga565bc80dabd392f5a1cb900768b21db4) |  |  | hermitian rank 2 operation |
| [ZHPR2]() |  |  | hermitian packed rank 2 operation |


## Level 3
| Netlib | Stdlib | WASM | Description |
| ------ | ------ | ---- | ----------- |
| [SGEMM]() | [sgemm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/sgemm) |  | matrix matrix multiply |
| [SSYMM]() |  |  | symmetric matrix matrix multiply |
| [SSYRK]() |  |  | symmetric rank-k update to a matrix |
| [SSYR2K](https://www.netlib.org/lapack/explore-html/dd/de5/group__her2_ga6741f2ac8fe025042fd994ccc6625b45.html#ga6741f2ac8fe025042fd994ccc6625b45) |  |  | symmetric rank-2k update to a matrix |
| [STRMM]() |  |  | triangular matrix matrix multiply |
| [STRSM]() |  |  | solving triangular matrix with multiple right hand sides |
| [DGEMM]() | [dgemm](https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/@stdlib/blas/base/dgemm) |  | matrix matrix multiply |
| [DSYMM]() |  |  | symmetric matrix matrix multiply |
| [DSYRK]() |  |  | symmetric rank-k update to a matrix |
| [DSYR2K]() |  |  | symmetric rank-2k update to a matrix |
| [DTRMM]() |  |  | triangular matrix matrix multiply |
| [DTRSM]() |  |  | solving triangular matrix with multiple right hand sides |
| [CGEMM]() |  |  | matrix matrix multiply |
| [CSYMM]() |  |  | symmetric matrix matrix multiply |
| [CHEMM]() |  |  | hermitian matrix matrix multiply |
| [CSYRK]() |  |  | symmetric rank-k update to a matrix |
| [CHERK]() |  |  | hermitian rank-k update to a matrix |
| [CSYR2K]() |  |  | symmetric rank-2k update to a matrix |
| [CHER2K]() |  |  | hermitian rank-2k update to a matrix |
| [CTRMM]() |  |  | triangular matrix matrix multiply |
| [CTRSM]() |  |  | solving triangular matrix with multiple right hand sides |
| [ZGEMM]() |  |  | matrix matrix multiply |
| [ZSYMM]() |  |  | symmetric matrix matrix multiply |
| [ZHEMM]() |  |  | hermitian matrix matrix multiply |
| [ZSYRK]() |  |  | symmetric rank-k update to a matrix |
| [ZHERK]() |  |  | hermitian rank-k update to a matrix |
| [ZSYR2K]() |  |  | symmetric rank-2k update to a matrix |
| [ZHER2K]() |  |  | hermitian rank-2k update to a matrix |
| [ZTRMM]() |  |  | triangular matrix matrix multiply |
| [ZTRSM]() |  |  | solving triangular matrix with multiple right hand sides |

