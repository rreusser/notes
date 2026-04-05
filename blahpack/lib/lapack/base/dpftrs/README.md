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

# dpftrs

> Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var dpftrs = require( '@stdlib/lapack/base/dpftrs' );
```

#### dpftrs( transr, uplo, N, nrhs, A, B, LDB )

Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **transr**: `transr`.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **nrhs**: number of right-hand sides.
-   **A**: input array `A`.
-   **B**: input array `B`.
-   **LDB**: leading dimension of `B`.

#### dpftrs.ndarray( transr, uplo, N, nrhs, A, strideA, offsetA, B, strideB1, strideB2, offsetB )

Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed format, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.
-   **strideB1**: stride of dimension 1 of `B`.
-   **strideB2**: stride of dimension 2 of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dpftrs()` corresponds to the [LAPACK][lapack] level routine [`dpftrs`][lapack-dpftrs].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dpftrs = require( '@stdlib/lapack/base/dpftrs' );

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

[lapack-dpftrs]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dpftrs.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->