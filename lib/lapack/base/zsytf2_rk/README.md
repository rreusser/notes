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

# zsytf2_rk

> Factorizes a complex symmetric matrix A using the bounded Bunch-Kaufman (rook) diagonal pivoting method (_rk format).

<section class="usage">

## Usage

```javascript
var zsytf2_rk = require( '@stdlib/lapack/base/zsytf2_rk' );
```

#### zsytf2_rk( order, uplo, N, A, LDA, e, strideE, IPIV, strideIPIV, offsetIPIV )

Factorizes a complex symmetric matrix A using the bounded Bunch-Kaufman (rook) diagonal pivoting method (_rk format).

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4, 0.5, 0, 0, 0, 0, -3, 1 ] );
var e = new Complex128Array( 2 );
var ipiv = new Int32Array( 2 );

zsytf2_rk( 'column-major', 'lower', 2, A, 2, e, 1, ipiv, 1, 0 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **IPIV**: output array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

#### zsytf2_rk.ndarray( uplo, N, A, strideA1, strideA2, offsetA, e, strideE, offsetE, IPIV, strideIPIV, offsetIPIV )

Factorizes a complex symmetric matrix A using the bounded Bunch-Kaufman (rook) diagonal pivoting method (_rk format)., using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4, 0.5, 0, 0, 0, 0, -3, 1 ] );
var e = new Complex128Array( 2 );
var ipiv = new Int32Array( 2 );

zsytf2_rk.ndarray( 'lower', 2, A, 1, 2, 0, e, 1, 0, ipiv, 1, 0 );
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
-   **IPIV**: output array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Computes `A = P*U*D*(U^T)*(P^T)` (upper) or `A = P*L*D*(L^T)*(P^T)` (lower), where `D` is complex symmetric block diagonal with `1x1` and `2x2` blocks. Off-diagonal entries of `D` are returned in `e`.
-   Unlike `zhetf2_rk` (the Hermitian variant), the diagonal of `A` is fully complex on input and output — no conjugation or real-part enforcement is performed.
-   `IPIV` uses 0-based indexing: non-negative values denote `1x1` pivots; negative values (`~kp`) denote `2x2` blocks.
-   This is the unblocked Level-2 kernel typically called by `zsytrf_rk`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytf2_rk = require( '@stdlib/lapack/base/zsytf2_rk' );

var A = new Complex128Array( [ 4, 0.5, 0, 0, 0, 0, -3, 1 ] );
var e = new Complex128Array( 2 );
var ipiv = new Int32Array( 2 );

var info = zsytf2_rk( 'column-major', 'lower', 2, A, 2, e, 1, ipiv, 1, 0 );
console.log( info );
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
