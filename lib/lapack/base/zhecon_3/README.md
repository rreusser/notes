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

# zhecon_3

> Estimates the reciprocal of the 1-norm condition number of a complex Hermitian matrix using the factorization computed by zhetrf_rk

<section class="usage">

## Usage

```javascript
var zhecon3 = require( '@stdlib/lapack/base/zhecon_3' );
```

#### zhecon_3( order, uplo, N, A, LDA, e, strideE, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK )

Estimates the reciprocal of the 1-norm condition number of a complex Hermitian matrix using the factorization computed by zhetrf_rk

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
var e = new Complex128Array( 3 );
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var work = new Complex128Array( 6 );
var rcond = new Float64Array( 1 );

zhecon3( 'column-major', 'upper', 3, A, 3, e, 1, IPIV, 1, 4.0, rcond, work, 1 );
// rcond[ 0 ] => 1
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **anorm**: anorm.
-   **rcond**: rcond.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.

#### zhecon_3.ndarray( uplo, N, A, strideA1, strideA2, offsetA, e, strideE, offsetE, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK )

Estimates the reciprocal of the 1-norm condition number of a complex Hermitian matrix using the factorization computed by zhetrf_rk, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
var e = new Complex128Array( 3 );
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var work = new Complex128Array( 6 );
var rcond = new Float64Array( 1 );

zhecon3.ndarray( 'upper', 3, A, 1, 3, 0, e, 1, 0, IPIV, 1, 0, 4.0, rcond, work, 1, 0 );
// rcond[ 0 ] => 1
```

The function has the following additional parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `E`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **anorm**: anorm.
-   **rcond**: rcond.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization `A = P*U*D*U^H*P^T` (or `A = P*L*D*L^H*P^T`) must be precomputed by `zhetrf_rk`.
-   The estimator uses Higham's 1-norm reverse-communication algorithm (`zlacn2`).
-   For Hermitian matrices, the diagonal of `A` is real, so the singular-`D` check examines the real part only.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zhecon3 = require( '@stdlib/lapack/base/zhecon_3' );

var A = new Complex128Array( [ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
var e = new Complex128Array( 3 );
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var work = new Complex128Array( 6 );
var rcond = new Float64Array( 1 );

zhecon3( 'column-major', 'upper', 3, A, 3, e, 1, IPIV, 1, 4.0, rcond, work, 1 );
console.log( rcond[ 0 ] );
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
