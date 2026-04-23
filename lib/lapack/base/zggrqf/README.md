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

# zggrqf

> Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B.

<section class="usage">

## Usage

```javascript
var zggrqf = require( '@stdlib/lapack/base/zggrqf' );
```

#### zggrqf( M, p, N, A, LDA, TAUA, strideTAUA, B, LDB, TAUB, strideTAUB, WORK, strideWORK, lwork )

Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **M**: number of rows.
-   **p**: `p`.
-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **TAUA**: input array `TAUA`.
-   **strideTAUA**: stride length for `TAUA`.
-   **B**: input array `B`.
-   **LDB**: leading dimension of `B`.
-   **TAUB**: input array `TAUB`.
-   **strideTAUB**: stride length for `TAUB`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **lwork**: `lwork`.

#### zggrqf.ndarray( M, p, N, A, strideA1, strideA2, offsetA, TAUA, strideTAUA, offsetTAUA, B, strideB1, strideB2, offsetB, TAUB, strideTAUB, offsetTAUB, WORK, strideWORK, offsetWORK, lwork )

Computes a generalized RQ factorization of an M-by-N complex matrix A and a P-by-N complex matrix B, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetTAUA**: starting index for `TAUA`.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.
-   **offsetTAUB**: starting index for `TAUB`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zggrqf()` corresponds to the [LAPACK][lapack] level routine [`zggrqf`][lapack-zggrqf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zggrqf = require( '@stdlib/lapack/base/zggrqf' );

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

[lapack-zggrqf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zggrqf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->