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

# zlatrs

> CABS1: |re(z)| + |im(z)|.

<section class="usage">

## Usage

```javascript
var zlatrs = require( '@stdlib/lapack/base/zlatrs' );
```

#### zlatrs( v, idx )

CABS1: |re(z)| + |im(z)|.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **v**: `v`.
-   **idx**: `idx`.

#### zlatrs.ndarray( uplo, trans, diag, normin, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

CABS1: |re(z)| + |im(z)|, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **trans**: specifies whether the matrix should be transposed.
-   **diag**: specifies whether the matrix is unit triangular.
-   **normin**: `normin`.
-   **N**: number of columns.
-   **A**: input array `A`.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **offsetX**: starting index for `X`.
-   **scale**: `scale`.
-   **CNORM**: input array `CNORM`.
-   **strideCNORM**: stride length for `CNORM`.
-   **offsetCNORM**: starting index for `CNORM`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlatrs()` corresponds to the [LAPACK][lapack] level routine [`zlatrs`][lapack-zlatrs].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlatrs = require( '@stdlib/lapack/base/zlatrs' );

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

[lapack-zlatrs]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlatrs.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->