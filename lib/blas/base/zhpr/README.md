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

# zhpr

> Performs the Hermitian packed rank-1 update `A := alpha*x*x**H + A`.

<section class="usage">

## Usage

```javascript
var zhpr = require( '@stdlib/blas/base/zhpr' );
```

#### zhpr( uplo, N, alpha, x, strideX, AP, strideAP )

Performs the Hermitian packed rank-1 update `A := alpha*x*x**H + A`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **alpha**: scalar constant.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **AP**: input array `AP`.
-   **strideAP**: stride length for `AP`.

#### zhpr.ndarray( uplo, N, alpha, x, strideX, offsetX, AP, strideAP, offsetAP )

Performs the Hermitian packed rank-1 update `A := alpha*x*x**H + A`, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetX**: starting index for `X`.
-   **offsetAP**: starting index for `AP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhpr()` corresponds to the [LAPACK][lapack] level routine [`zhpr`][lapack-zhpr].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zhpr = require( '@stdlib/blas/base/zhpr' );

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

[lapack-zhpr]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zhpr.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->