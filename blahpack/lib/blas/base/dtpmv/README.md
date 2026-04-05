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

# dtpmv

> Performs one of the matrix-vector operations `x := A*x` or `x := A^T*x`.

<section class="usage">

## Usage

```javascript
var dtpmv = require( '@stdlib/blas/base/dtpmv' );
```

#### dtpmv( uplo, trans, diag, N, AP, strideAP, x, strideX )

Performs one of the matrix-vector operations `x := A*x` or `x := A^T*x`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **trans**: specifies whether the matrix should be transposed.
-   **diag**: specifies whether the matrix is unit triangular.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **strideAP**: stride length for `AP`.
-   **x**: `x`.
-   **strideX**: stride length for `X`.

#### dtpmv.ndarray( uplo, trans, diag, N, AP, strideAP, offsetAP, x, strideX, offsetX )

Performs one of the matrix-vector operations `x := A*x` or `x := A^T*x`, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetAP**: starting index for `AP`.
-   **offsetX**: starting index for `X`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dtpmv()` corresponds to the [LAPACK][lapack] level routine [`dtpmv`][lapack-dtpmv].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dtpmv = require( '@stdlib/blas/base/dtpmv' );

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

[lapack-dtpmv]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dtpmv.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->