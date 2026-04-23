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

# dgetc2

> Performs LU factorization with complete pivoting of a general N-by-N matrix.

<section class="usage">

## Usage

```javascript
var dgetc2 = require( '@stdlib/lapack/base/dgetc2' );
```

#### dgetc2( N, A, LDA, IPIV, strideIPIV, JPIV, strideJPIV )

Performs LU factorization with complete pivoting of a general N-by-N matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **IPIV**: input array `IPIV`.
-   **strideIPIV**: stride length for `IPIV`.
-   **JPIV**: input array `JPIV`.
-   **strideJPIV**: stride length for `JPIV`.

#### dgetc2.ndarray( N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV )

Performs LU factorization with complete pivoting of a general N-by-N matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **offsetJPIV**: starting index for `JPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgetc2()` corresponds to the [LAPACK][lapack] level routine [`dgetc2`][lapack-dgetc2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dgetc2 = require( '@stdlib/lapack/base/dgetc2' );

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

[lapack-dgetc2]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dgetc2.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->