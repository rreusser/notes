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

# zlagtm

> Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.

<section class="usage">

## Usage

```javascript
var zlagtm = require( '@stdlib/lapack/base/zlagtm' );
```

#### zlagtm( trans, N, nrhs, alpha, DL, strideDL, d, strideD, DU, strideDU, X, LDX, beta, B, LDB )

Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **trans**: specifies whether the matrix should be transposed.
-   **N**: number of columns.
-   **nrhs**: number of right-hand sides.
-   **alpha**: scalar constant.
-   **DL**: input array `DL`.
-   **strideDL**: stride length for `DL`.
-   **d**: `d`.
-   **strideD**: stride length for `D`.
-   **DU**: input array `DU`.
-   **strideDU**: stride length for `DU`.
-   **X**: input array `X`.
-   **LDX**: leading dimension of `X`.
-   **beta**: scalar constant.
-   **B**: input array `B`.
-   **LDB**: leading dimension of `B`.

#### zlagtm.ndarray( trans, N, nrhs, alpha, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, X, strideX1, strideX2, offsetX, beta, B, strideB1, strideB2, offsetB )

Perform a matrix-vector product of the form C := alpha*A*B + beta*C where A is a complex tridiagonal matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetDL**: starting index for `DL`.
-   **offsetD**: starting index for `D`.
-   **offsetDU**: starting index for `DU`.
-   **strideX1**: stride of dimension 1 of `X`.
-   **strideX2**: stride of dimension 2 of `X`.
-   **offsetX**: starting index for `X`.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlagtm()` corresponds to the [LAPACK][lapack] level routine [`zlagtm`][lapack-zlagtm].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlagtm = require( '@stdlib/lapack/base/zlagtm' );

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

[lapack-zlagtm]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlagtm.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->