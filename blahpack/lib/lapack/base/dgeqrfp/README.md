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

# dgeqrfp

> Computes a QR factorization of a real M-by-N matrix A = Q * R with non-negative diagonal elements of R, using a blocked algorithm.

<section class="usage">

## Usage

```javascript
var dgeqrfp = require( '@stdlib/lapack/base/dgeqrfp' );
```

#### dgeqrfp( order, M, N, A, LDA, TAU, strideTAU, WORK, strideWORK )

Computes a QR factorization of a real M-by-N matrix A = Q * R with non-negative diagonal elements of R.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 96 );

dgeqrfp( 'column-major', 3, 3, A, 3, TAU, 1, WORK, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix `A`.
-   **LDA**: leading dimension of `A`.
-   **TAU**: output array of scalar factors.
-   **strideTAU**: stride length for `TAU`.
-   **WORK**: workspace array.
-   **strideWORK**: stride length for `WORK`.

On exit, the diagonal elements of `R` are non-negative.

#### dgeqrfp.ndarray( M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK )

Computes a QR factorization of a real M-by-N matrix A = Q * R with non-negative diagonal elements of R, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 96 );

dgeqrfp.ndarray( 3, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetTAU**: starting index for `TAU`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgeqrfp()` corresponds to the [LAPACK][lapack] level routine [`dgeqrfp`][lapack-dgeqrfp].
-   This routine differs from `dgeqrf` only in that `R` is guaranteed to have non-negative diagonal elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dgeqrfp = require( '@stdlib/lapack/base/dgeqrfp' );

var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );
var TAU = new Float64Array( 3 );
var WORK = new Float64Array( 96 );

var info = dgeqrfp( 'column-major', 3, 3, A, 3, TAU, 1, WORK, 1 );
console.log( info );
console.log( A );
console.log( TAU );
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

[lapack-dgeqrfp]: https://www.netlib.org/lapack/explore-html/

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
