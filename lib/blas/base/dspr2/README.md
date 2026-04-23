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

# dspr2

> Performs the symmetric rank-2 operation `A := alpha*x*y^T + alpha*y*x^T + A`.

<section class="usage">

## Usage

```javascript
var dspr2 = require( '@stdlib/blas/base/dspr2' );
```

#### dspr2( uplo, N, alpha, x, strideX, y, strideY, AP, strideAP )

Performs the symmetric rank-2 operation `A := alpha*x*y^T + alpha*y*x^T + A`.

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
-   **y**: `y`.
-   **strideY**: stride length for `Y`.
-   **AP**: input array `AP`.
-   **strideAP**: stride length for `AP`.

#### dspr2.ndarray( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, AP, strideAP, offsetAP )

Performs the symmetric rank-2 operation `A := alpha*x*y^T + alpha*y*x^T + A`, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetX**: starting index for `X`.
-   **offsetY**: starting index for `Y`.
-   **offsetAP**: starting index for `AP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dspr2()` corresponds to the [LAPACK][lapack] level routine [`dspr2`][lapack-dspr2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dspr2 = require( '@stdlib/blas/base/dspr2' );

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

[lapack-dspr2]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dspr2.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->