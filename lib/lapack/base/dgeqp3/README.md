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

# dgeqp3

> Computes a QR factorization with column pivoting of a real M-by-N matrix:.

<section class="usage">

## Usage

```javascript
var dgeqp3 = require( '@stdlib/lapack/base/dgeqp3' );
```

#### dgeqp3( M, N, A, LDA, JPVT, strideJPVT, TAU, strideTAU )

Computes a QR factorization with column pivoting of a real M-by-N matrix:.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **JPVT**: input array `JPVT`.
-   **strideJPVT**: stride length for `JPVT`.
-   **TAU**: input array `TAU`.
-   **strideTAU**: stride length for `TAU`.

#### dgeqp3.ndarray( M, N, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK, lwork )

Computes a QR factorization with column pivoting of a real M-by-N matrix:, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetJPVT**: starting index for `JPVT`.
-   **offsetTAU**: starting index for `TAU`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: `lwork`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgeqp3()` corresponds to the [LAPACK][lapack] level routine [`dgeqp3`][lapack-dgeqp3].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dgeqp3 = require( '@stdlib/lapack/base/dgeqp3' );

// TODO: Add examples
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

[lapack-dgeqp3]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dgeqp3.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->