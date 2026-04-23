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

# dgsvj0

> Pre-processor for dgesvj performing Jacobi rotations

<section class="usage">

## Usage

```javascript
var dgsvj0 = require( '@stdlib/lapack/base/dgsvj0' );
```

#### dgsvj0( order, jobv, M, N, A, LDA, d, strideD, sva, strideSVA, mv, V, LDV, eps, sfmin, tol, nsweep, work, strideWORK, lwork )

Pre-processor for dgesvj performing Jacobi rotations

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **jobv**: jobv.
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **sva**: input array.
-   **strideSVA**: stride length for `sva`.
-   **mv**: mv.
-   **V**: output matrix.
-   **LDV**: leading dimension of `V`.
-   **eps**: eps.
-   **sfmin**: sfmin.
-   **tol**: tol.
-   **nsweep**: nsweep.
-   **work**: input array.
-   **strideWORK**: stride length for `work`.
-   **lwork**: lwork.

#### dgsvj0.ndarray( jobv, M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD, sva, strideSVA, offsetSVA, mv, V, strideV1, strideV2, offsetV, eps, sfmin, tol, nsweep, work, strideWORK, offsetWORK, lwork )

Pre-processor for dgesvj performing Jacobi rotations, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **jobv**: jobv.
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.
-   **sva**: input array.
-   **strideSVA**: stride length for `sva`.
-   **offsetSVA**: starting index for `SVA`.
-   **mv**: mv.
-   **V**: output matrix.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
-   **eps**: eps.
-   **sfmin**: sfmin.
-   **tol**: tol.
-   **nsweep**: nsweep.
-   **work**: input array.
-   **strideWORK**: stride length for `work`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
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

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
