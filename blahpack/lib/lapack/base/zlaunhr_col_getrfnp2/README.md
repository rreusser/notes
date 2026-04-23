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

# zlaunhr_col_getrfnp2

> modified LU factorization without pivoting of a complex general M-by-N matrix A (recursive)

<section class="usage">

## Usage

```javascript
var zlaunhr_col_getrfnp2 = require( '@stdlib/lapack/base/zlaunhr_col_getrfnp2' );
```

#### zlaunhr_col_getrfnp2( order, M, N, A, LDA, d, strideD )

modified LU factorization without pivoting of a complex general M-by-N matrix A (recursive)

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp2( 'column-major', 3, 3, A, 3, D, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input/output complex matrix ([`Complex128Array`][@stdlib/array/complex128]).
-   **LDA**: leading dimension of `A`.
-   **d**: output diagonal sign array ([`Complex128Array`][@stdlib/array/complex128]).
-   **strideD**: stride length for `d`.

#### zlaunhr_col_getrfnp2.ndarray( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD )

modified LU factorization without pivoting of a complex general M-by-N matrix A (recursive), using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp2.ndarray( 3, 3, A, 1, 3, 0, D, 1, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **d**: output array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A - S = L*U`, where `S` is a diagonal sign matrix with `D(i) = -sign(Re(A(i,i)))` (after `i-1` Gaussian elimination steps). Because `|D(i,i)| = 1`, no pivoting is needed for stability.
-   `A` is overwritten with the unit-lower-triangular factor `L` (below the diagonal) and the upper triangular factor `U` (on and above the diagonal). The diagonal entries of `U` are stored negated.
-   This is an auxiliary routine used by `zunhr_col` for Householder reconstruction.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlaunhr_col_getrfnp2 = require( '@stdlib/lapack/base/zlaunhr_col_getrfnp2' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp2( 'column-major', 3, 3, A, 3, D, 1 );
console.log( A );
console.log( D );
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
