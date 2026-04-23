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

# dorhr_col

> Reconstruct the Householder vectors and block reflectors of a compact-WY TSQR factorization.

<section class="usage">

## Usage

```javascript
var dorhr_col = require( '@stdlib/lapack/base/dorhr_col' );
```

#### dorhr_col( order, M, N, nb, A, LDA, T, LDT, d, strideD )

Given an M-by-N matrix with orthonormal columns, reconstructs the
Householder vectors `V` (overwriting `A`), the block reflectors `T`, and
the diagonal sign vector `D` (entries `±1`) such that the input `Q` equals
`(I - V*T*V^T) * diag(D)`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([
    0.5, 0.5, 0.5, 0.5,
    0.5, -0.5, 0.5, -0.5
]);
var T = new Float64Array( 4 );
var d = new Float64Array( 2 );

dorhr_col( 'column-major', 4, 2, 2, A, 4, T, 2, d, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **nb**: block size.
-   **A**: input/output matrix (overwritten with Householder vectors `V`).
-   **LDA**: leading dimension of `A`.
-   **T**: output block reflector matrix.
-   **LDT**: leading dimension of `T`.
-   **d**: output diagonal sign vector (entries are `±1`).
-   **strideD**: stride length for `d`.

#### dorhr_col.ndarray( M, N, nb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, d, strideD, offsetD )

Same as above, using stride/offset indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([ 0.5, 0.5, 0.5, 0.5, 0.5, -0.5, 0.5, -0.5 ]);
var T = new Float64Array( 4 );
var d = new Float64Array( 2 );

dorhr_col.ndarray( 4, 2, 2, A, 1, 4, 0, T, 1, 2, 0, d, 1, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **nb**: nb.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **T**: input matrix.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **d**: output array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `nb` is the block size used to build `T` and must satisfy `nb >= 1`. `LDT` must be at least `max(1, min(nb, N))`.
-   On input, `A` must have orthonormal columns (it is typically the `Q` factor of a tall-skinny QR).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dorhr_col = require( '@stdlib/lapack/base/dorhr_col' );

var A = new Float64Array([ 0.5, 0.5, 0.5, 0.5, 0.5, -0.5, 0.5, -0.5 ]);
var T = new Float64Array( 4 );
var d = new Float64Array( 2 );

dorhr_col( 'column-major', 4, 2, 2, A, 4, T, 2, d, 1 );
console.log( 'V:', A );
console.log( 'T:', T );
console.log( 'D:', d );
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
