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

# zhfrk

> Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var zhfrk = require( '@stdlib/lapack/base/zhfrk' );
```

#### zhfrk( transr, uplo, trans, N, K, alpha, A, LDA, beta, C )

Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **transr**: `transr`.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **trans**: specifies whether the matrix should be transposed.
-   **N**: number of columns.
-   **K**: inner dimension.
-   **alpha**: scalar constant.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **beta**: scalar constant.
-   **C**: input array `C`.

#### zhfrk.ndarray( transr, uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC, offsetC )

Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **strideC**: stride length for `C`.
-   **offsetC**: starting index for `C`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhfrk()` corresponds to the [LAPACK][lapack] level routine [`zhfrk`][lapack-zhfrk].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zhfrk = require( '@stdlib/lapack/base/zhfrk' );

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

[lapack-zhfrk]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zhfrk.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->