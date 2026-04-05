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

# zggqrf

> Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B.

<section class="usage">

## Usage

```javascript
var zggqrf = require( '@stdlib/lapack/base/zggqrf' );
```

#### zggqrf( N, M, p, A, LDA, TAUA, strideTAUA, B, LDB, TAUB, strideTAUB, WORK, strideWORK, lwork )

Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **M**: number of rows.
-   **p**: `p`.
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

#### zggqrf.ndarray( N, M, p, A, strideA1, strideA2, offsetA, TAUA, strideTAUA, offsetTAUA, B, strideB1, strideB2, offsetB, TAUB, strideTAUB, offsetTAUB, WORK, strideWORK, offsetWORK, lwork )

Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B, using alternative indexing semantics.

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

-   `zggqrf()` corresponds to the [LAPACK][lapack] level routine [`zggqrf`][lapack-zggqrf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zggqrf = require( '@stdlib/lapack/base/zggqrf' );

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

[lapack-zggqrf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zggqrf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->