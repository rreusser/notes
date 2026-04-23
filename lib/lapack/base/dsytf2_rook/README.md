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

# dsytf2_rook

> Bounded Bunch-Kaufman (rook) factorization of a symmetric indefinite matrix

<section class="usage">

## Usage

```javascript
var dsytf2_rook = require( '@stdlib/lapack/base/dsytf2_rook' );
```

#### dsytf2_rook( order, uplo, N, A, LDA, IPIV, strideIPIV, offsetIPIV )

Bounded Bunch-Kaufman (rook) factorization of a symmetric indefinite matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 2.0, -1.0, 0.0, 2.0 ] );
var IPIV = new Int32Array( 2 );

dsytf2_rook( 'column-major', 'upper', 2, A, 2, IPIV, 1, 0 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the matrix triangle (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **A**: input/output matrix.
-   **LDA**: leading dimension of `A`.
-   **IPIV**: output pivot index array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

#### dsytf2_rook.ndarray( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Bounded Bunch-Kaufman (rook) factorization of a symmetric indefinite matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Float64Array( [ 2.0, -1.0, 0.0, 2.0 ] );
var IPIV = new Int32Array( 2 );

dsytf2_rook.ndarray( 'upper', 2, A, 1, 2, 0, IPIV, 1, 0 );
```

The function has the following additional parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **IPIV**: output array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Computes the factorization `A = U*D*U^T` or `A = L*D*L^T`, where `D` is block-diagonal with 1-by-1 and 2-by-2 blocks and `U` (or `L`) is a product of permutation and unit upper (lower) triangular matrices.
-   Unlike the classical Bunch-Kaufman pivoting in `dsytf2`, the "rook" variant performs a bounded pivot search (iterating rows and columns) which yields smaller element growth.
-   `IPIV` uses the rook encoding: positive values are 0-based pivots for 1-by-1 blocks; for a 2-by-2 block, BOTH entries are negative and each encodes its own swap target via bitwise NOT (`~IPIV[k]` gives the 0-based row/column that was swapped with `k`).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytf2_rook = require( '@stdlib/lapack/base/dsytf2_rook' );

var A = new Float64Array([
    2.0, -1.0, 0.0, 0.0,
    0.0, 2.0, -1.0, 0.0,
    0.0, 0.0, 2.0, -1.0,
    0.0, 0.0, 0.0, 2.0
]);
var IPIV = new Int32Array( 4 );

var info = dsytf2_rook( 'column-major', 'lower', 4, A, 4, IPIV, 1, 0 );
console.log( info );
console.log( A );
console.log( IPIV );
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
