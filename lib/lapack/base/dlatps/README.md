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

# dlatps

> @license Apache-2.0.

<section class="usage">

## Usage

```javascript
var dlatps = require( '@stdlib/lapack/base/dlatps' );
```

#### dlatps( uplo, trans, diag, normin, N, AP, x, strideX, scale, CNORM, strideCNORM )

@license Apache-2.0.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **trans**: specifies whether the matrix should be transposed.
-   **diag**: specifies whether the matrix is unit triangular.
-   **normin**: `normin`.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **scale**: `scale`.
-   **CNORM**: input array `CNORM`.
-   **strideCNORM**: stride length for `CNORM`.

#### dlatps.ndarray( uplo, trans, diag, normin, N, AP, strideAP, offsetAP, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

@license Apache-2.0, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **offsetX**: starting index for `X`.
-   **offsetCNORM**: starting index for `CNORM`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlatps()` corresponds to the [LAPACK][lapack] level routine [`dlatps`][lapack-dlatps].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlatps = require( '@stdlib/lapack/base/dlatps' );

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

[lapack-dlatps]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlatps.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->