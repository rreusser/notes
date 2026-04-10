<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# dtgsna

> Estimates reciprocal condition numbers for eigenvalues and eigenvectors of a generalized Schur form

<section class="usage">

## Usage

```javascript
var dtgsna = require( '@stdlib/lapack/base/dtgsna' );
```

#### dtgsna( order, job, howmny, SELECT, strideSELECT, N, A, LDA, B, LDB, VL, LDVL, VR, LDVR, s, strideS, DIF, strideDIF, mm, M, WORK, strideWORK, lwork, IWORK, strideIWORK, offsetIWORK )

Estimates reciprocal condition numbers for eigenvalues and eigenvectors of a generalized Schur form

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Uint8Array = require( '@stdlib/array/uint8' );

var A = new Float64Array( [ 1.0, 0.0, 0.5, 2.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.2, 1.5 ] );
var VL = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var VR = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var s = new Float64Array( 2 );
var DIF = new Float64Array( 2 );
var WORK = new Float64Array( 64 );
var IWORK = new Int32Array( 16 );
var SELECT = new Uint8Array( 2 );
var M = new Int32Array( 1 );

dtgsna( 'column-major', 'both', 'all', SELECT, 1, 2, A, 2, B, 2, VL, 2, VR, 2, s, 1, DIF, 1, 2, M, WORK, 1, WORK.length, IWORK, 1, 0 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **job**: specifies the operation type.
-   **howmny**: specifies the operation type.
-   **SELECT**: input array.
-   **strideSELECT**: stride length for `SELECT`.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **B**: input matrix.
-   **LDB**: leading dimension of `B`.
-   **VL**: input matrix.
-   **LDVL**: leading dimension of `VL`.
-   **VR**: input matrix.
-   **LDVR**: leading dimension of `VR`.
-   **s**: input array.
-   **strideS**: stride length for `s`.
-   **DIF**: input array.
-   **strideDIF**: stride length for `DIF`.
-   **mm**: mm.
-   **M**: number of rows.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **lwork**: lwork.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

#### dtgsna.ndarray( job, howmny, SELECT, strideSELECT, offsetSELECT, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, s, strideS, offsetS, DIF, strideDIF, offsetDIF, mm, M, WORK, strideWORK, offsetWORK, lwork, IWORK, strideIWORK, offsetIWORK )

Estimates reciprocal condition numbers for eigenvalues and eigenvectors of a generalized Schur form, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Uint8Array = require( '@stdlib/array/uint8' );

var A = new Float64Array( [ 1.0, 0.0, 0.5, 2.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.2, 1.5 ] );
var VL = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var VR = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var s = new Float64Array( 2 );
var DIF = new Float64Array( 2 );
var WORK = new Float64Array( 64 );
var IWORK = new Int32Array( 16 );
var SELECT = new Uint8Array( 2 );
var M = new Int32Array( 1 );

dtgsna.ndarray( 'both', 'all', SELECT, 1, 0, 2, A, 1, 2, 0, B, 1, 2, 0, VL, 1, 2, 0, VR, 1, 2, 0, s, 1, 0, DIF, 1, 0, 2, M, WORK, 1, 0, WORK.length, IWORK, 1, 0 );
```

The function has the following additional parameters:

-   **job**: specifies the operation type.
-   **howmny**: specifies the operation type.
-   **SELECT**: input array.
-   **strideSELECT**: stride length for `SELECT`.
-   **offsetSELECT**: starting index for `SELECT`.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.
-   **VL**: input matrix.
-   **strideVL1**: stride of dimension 1 of `VL`.
-   **strideVL2**: stride of dimension 2 of `VL`.
-   **offsetVL**: starting index for `VL`.
-   **VR**: input matrix.
-   **strideVR1**: stride of dimension 1 of `VR`.
-   **strideVR2**: stride of dimension 2 of `VR`.
-   **offsetVR**: starting index for `VR`.
-   **s**: input array.
-   **strideS**: stride length for `s`.
-   **offsetS**: starting index for `S`.
-   **DIF**: input array.
-   **strideDIF**: stride length for `DIF`.
-   **offsetDIF**: starting index for `DIF`.
-   **mm**: mm.
-   **M**: number of rows.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine assumes the matrix pair `(A,B)` is already in generalized real Schur canonical form (upper quasi-triangular `A`, upper triangular `B`).
-   For `job='eigenvalues'` or `'both'`, the left and right eigenvectors `VL` and `VR` must be provided (e.g. from `dtgevc`).
-   The workspace must satisfy `lwork >= 2*N*(N+2)+16` when `job='eigenvectors'` or `'both'`; otherwise `lwork >= N` suffices.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Uint8Array = require( '@stdlib/array/uint8' );
var dtgsna = require( '@stdlib/lapack/base/dtgsna' );

var N = 3;
var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.3, 0.4, 3.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 1.5, 0.0, 0.1, 0.3, 2.0 ] );
var VL = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
var VR = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
var s = new Float64Array( N );
var DIF = new Float64Array( N );
var WORK = new Float64Array( 2 * N * ( N + 2 ) + 16 );
var IWORK = new Int32Array( N + 6 );
var SELECT = new Uint8Array( N );
var M = new Int32Array( 1 );

dtgsna.ndarray( 'both', 'all', SELECT, 1, 0, N, A, 1, N, 0, B, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, s, 1, 0, DIF, 1, 0, N, M, WORK, 1, 0, WORK.length, IWORK, 1, 0 );
console.log( s );
console.log( DIF );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
