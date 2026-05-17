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

# zsytrf_rook

> Computes the factorization of a complex symmetric matrix using the bounded Bunch-Kaufman ("rook") diagonal pivoting method (blocked algorithm).

<section class="usage">

## Usage

```javascript
var zsytrfRook = require( '@stdlib/lapack/base/zsytrf_rook' );
```

#### zsytrfRook( order, uplo, N, A, LDA, IPIV, strideIPIV )

Computes the factorization of a complex symmetric matrix `A` using the bounded Bunch-Kaufman ("rook") diagonal pivoting method.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( 2 );

zsytrfRook( 'column-major', 'lower', 2, A, 2, IPIV, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies whether the upper or lower triangular part of `A` is referenced (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **A**: input/output complex symmetric matrix.
-   **LDA**: leading dimension of `A`.
-   **IPIV**: pivot index output array.
-   **strideIPIV**: stride length for `IPIV`.

#### zsytrfRook.ndarray( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV )

Computes the factorization, using alternative indexing semantics (no `order` parameter; explicit strides and offsets for `A` and `IPIV`).

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( 2 );

zsytrfRook.ndarray( 'lower', 2, A, 1, 2, 0, IPIV, 1, 0 );
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A` (in complex elements).
-   **strideA2**: stride of dimension 2 of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zsytrfRook()` corresponds to the [LAPACK][lapack] level routine [`zsytrf_rook`][lapack-zsytrf-rook].
-   `A` is treated as a **complex symmetric** matrix (`A = A^T`), not Hermitian. There is no conjugation at mirrored entries; diagonal entries may be fully complex.
-   On output, `A` is overwritten with the block diagonal factor `D` and the multipliers used to obtain it. The strictly lower (or upper) triangular part is the unit triangular factor `L` (or `U`).
-   Pivot indices are stored in `IPIV` using the stdlib bitwise-NOT convention: 1x1 pivots use non-negative 0-based indices; for a 2x2 pivot block, both entries are negative and each encodes its own swap target via bitwise NOT (i.e., `~IPIV[k]` gives the 0-based index of the row/column swapped with `k`). Because rook pivoting performs an extra row/column search per step, the two entries of a 2x2 block can encode different swap targets.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytrfRook = require( '@stdlib/lapack/base/zsytrf_rook' );

var A = new Complex128Array( [ 4.0, 1.0, 1.0, 2.0, 1.0, 2.0, 5.0, -1.0 ] );
var IPIV = new Int32Array( 2 );

var info = zsytrfRook.ndarray( 'lower', 2, A, 1, 2, 0, IPIV, 1, 0 );
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

[lapack]: https://www.netlib.org/lapack/explore-html/

[lapack-zsytrf-rook]: https://www.netlib.org/lapack/explore-html/d3/d4d/group__sytrf__rook.html

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
