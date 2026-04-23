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

# zhetf2_rook

> Bounded Bunch-Kaufman (rook) factorization of a complex Hermitian indefinite matrix

<section class="usage">

## Usage

```javascript
var zhetf2_rook = require( '@stdlib/lapack/base/zhetf2_rook' );
```

#### zhetf2_rook( order, uplo, N, A, LDA, IPIV, strideIPIV, offsetIPIV )

Bounded Bunch-Kaufman (rook) factorization of a complex Hermitian indefinite matrix

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 1.0, -2.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( 2 );

zhetf2_rook( 'column-major', 'upper', 2, A, 2, IPIV, 1, 0 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the matrix triangle (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **A**: input/output Hermitian matrix.
-   **LDA**: leading dimension of `A`.
-   **IPIV**: output pivot index array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

#### zhetf2_rook.ndarray( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Bounded Bunch-Kaufman (rook) factorization of a complex Hermitian indefinite matrix, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 1.0, -2.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( 2 );

zhetf2_rook.ndarray( 'upper', 2, A, 1, 2, 0, IPIV, 1, 0 );
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

-   Computes `A = U*D*U^H` or `A = L*D*L^H`, where `D` is Hermitian and block-diagonal with 1-by-1 and 2-by-2 blocks.
-   The "rook" variant performs bounded pivot search to limit element growth.
-   `IPIV` uses the rook encoding: non-negative values are 0-based pivot indices for 1x1 blocks; for a 2x2 block, BOTH entries are negative and each encodes its own swap target via bitwise NOT.
-   Diagonal elements of the Hermitian matrix `A` are assumed to be real on entry; the routine forces them to be real at each step.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhetf2_rook = require( '@stdlib/lapack/base/zhetf2_rook' );

var A = new Complex128Array([
    4, 0, 1, -2, 3, 1,
    0, 0, 5, 0, 2, -1,
    0, 0, 0, 0, 7, 0
]);
var IPIV = new Int32Array( 3 );

var info = zhetf2_rook( 'column-major', 'lower', 3, A, 3, IPIV, 1, 0 );
console.log( info );
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
