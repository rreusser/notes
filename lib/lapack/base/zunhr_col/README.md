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

# zunhr_col

> Reconstructs Householder reflector vectors and block reflector triangular factors from a unitary-on-input matrix `Q_in` (output of `zlatsqr`).

<section class="usage">

## Usage

```javascript
var zunhr_col = require( '@stdlib/lapack/base/zunhr_col' );
```

#### zunhr_col( order, M, N, nb, A, LDA, T, LDT, d, strideD )

Recovers the Householder vectors `V` (in-place in `A`) and block reflector matrices `T` from the columns of an M-by-N matrix that satisfies `Q_in^H * Q_in = I` (i.e. has orthonormal columns), splitting them into `nb`-column blocks. The diagonal sign vector `d` records the sign chosen for each column's reflector so that the original matrix can be reconstructed.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zunhr_col = require( '@stdlib/lapack/base/zunhr_col' );

var M = 3;
var N = 3;
var nb = 1;
var A = new Complex128Array( M * N );
var T = new Complex128Array( M * N );
var d = new Float64Array( N );

zunhr_col( 'column-major', M, N, nb, A, M, T, M, d, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **nb**: nb.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **T**: input matrix.
-   **LDT**: leading dimension of `T`.
-   **d**: output array.
-   **strideD**: stride length for `d`.

#### zunhr_col.ndarray( M, N, nb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, d, strideD, offsetD )

Recovers the Householder vectors and block reflector matrices, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zunhr_col = require( '@stdlib/lapack/base/zunhr_col' );

var M = 3;
var N = 3;
var nb = 1;
var A = new Complex128Array( M * N );
var T = new Complex128Array( M * N );
var d = new Float64Array( N );

zunhr_col( 'column-major', M, N, nb, A, M, T, M, d, 1 );
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

-   See LAPACK reference documentation for full algorithmic details.
</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zunhr_col = require( '@stdlib/lapack/base/zunhr_col' );

var M = 3;
var N = 3;
var nb = 1;
var A = new Complex128Array( M * N );
var T = new Complex128Array( M * N );
var d = new Float64Array( N );

zunhr_col( 'column-major', M, N, nb, A, M, T, M, d, 1 );
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
