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

# zgeqrt

> Compute a blocked QR factorization of a complex M-by-N matrix `A` using the compact WY representation of `Q`.

<section class="usage">

## Usage

```javascript
var zgeqrt = require( '@stdlib/lapack/base/zgeqrt' );
```

#### zgeqrt( order, M, N, nb, A, LDA, T, LDT, WORK )

Compute a blocked QR factorization of a complex M-by-N matrix using the compact WY representation of Q.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var WORK = new Complex128Array( 12 );

zgeqrt( 'column-major', 4, 3, 2, A, 4, T, 2, WORK );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of the matrix `A`.
-   **N**: number of columns of the matrix `A`.
-   **nb**: block size (`1 <= nb <= min(M,N)`).
-   **A**: input/output matrix; on exit, the upper triangle/trapezoid contains `R` and the strict lower triangle contains the Householder vectors `V`.
-   **LDA**: leading dimension of `A`.
-   **T**: output `nb`-by-`min(M,N)` matrix containing the upper triangular block reflectors `T1, T2, ..., TB` stored side-by-side.
-   **LDT**: leading dimension of `T`.
-   **WORK**: workspace array of length `>= nb*N`.

#### zgeqrt.ndarray( M, N, nb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK )

Compute a blocked QR factorization of a complex M-by-N matrix using the compact WY representation of Q, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var WORK = new Complex128Array( 12 );

zgeqrt.ndarray( 4, 3, 2, A, 1, 4, 0, T, 1, 2, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgeqrt()` corresponds to the [LAPACK][lapack] level routine [`zgeqrt`][lapack-zgeqrt].
-   Internally dispatches to the unblocked compact-WY kernel [`zgeqrt2`][@stdlib/lapack/base/zgeqrt2] per panel and applies the block reflector via [`zlarfb`][@stdlib/lapack/base/zlarfb] (using `'conjugate-transpose'`) to update the trailing matrix.
-   The block size `nb` is exposed as an explicit parameter; the LAPACK reference uses `ILAENV` to choose it. Callers that have no preference may pass `32`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgeqrt = require( '@stdlib/lapack/base/zgeqrt' );

var A = new Complex128Array( 12 );
var T = new Complex128Array( 6 );
var WORK = new Complex128Array( 12 );

zgeqrt.ndarray( 4, 3, 2, A, 1, 4, 0, T, 1, 2, 0, WORK, 1, 0 );

console.log( A );
console.log( T );
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

[lapack-zgeqrt]: https://www.netlib.org/lapack/explore-html/d8/d2c/group__geqrt.html

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

[@stdlib/lapack/base/zgeqrt2]: https://github.com/stdlib-js/stdlib

[@stdlib/lapack/base/zlarfb]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
