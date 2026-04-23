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

# dlatbs

> Solves a triangular banded system with scaling to prevent overflow.

<section class="usage">

## Usage

```javascript
var dlatbs = require( '@stdlib/lapack/base/dlatbs' );
```

#### dlatbs( uplo, trans, diag, normin, N, kd, AB, LDAB, x, strideX, scale, CNORM, strideCNORM )

Solves a triangular banded system with scaling to prevent overflow.

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
-   **kd**: `kd`.
-   **AB**: input array `AB`.
-   **LDAB**: leading dimension of `AB`.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **scale**: `scale`.
-   **CNORM**: input array `CNORM`.
-   **strideCNORM**: stride length for `CNORM`.

#### dlatbs.ndarray( uplo, trans, diag, normin, N, kd, AB, strideAB1, strideAB2, offsetAB, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

Solves a triangular banded system with scaling to prevent overflow, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAB1**: stride of dimension 1 of `AB`.
-   **strideAB2**: stride of dimension 2 of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **offsetX**: starting index for `X`.
-   **offsetCNORM**: starting index for `CNORM`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlatbs()` corresponds to the [LAPACK][lapack] level routine [`dlatbs`][lapack-dlatbs].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlatbs = require( '@stdlib/lapack/base/dlatbs' );

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

[lapack-dlatbs]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlatbs.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->