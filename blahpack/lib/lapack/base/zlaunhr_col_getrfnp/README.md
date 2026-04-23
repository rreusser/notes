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

# zlaunhr_col_getrfnp

> modified LU factorization without pivoting of a complex general M-by-N matrix (blocked driver)

<section class="usage">

## Usage

```javascript
var zlaunhr_col_getrfnp = require( '@stdlib/lapack/base/zlaunhr_col_getrfnp' );
```

#### zlaunhr_col_getrfnp( order, M, N, A, LDA, d, strideD )

Computes the modified LU factorization without pivoting of a complex general M-by-N matrix using a blocked right-looking algorithm.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp( 'column-major', 3, 3, A, 3, D, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input/output complex matrix.
-   **LDA**: leading dimension of `A`.
-   **d**: output diagonal sign array.
-   **strideD**: stride length for `d`.

#### zlaunhr_col_getrfnp.ndarray( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD )

Computes the modified LU factorization without pivoting of a complex general M-by-N matrix using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp.ndarray( 3, 3, A, 1, 3, 0, D, 1, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input/output complex matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **d**: output diagonal sign array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization has the form `A - S = L*U`, where `S` is a diagonal sign matrix with `D(i) = -sign(Re(A(i,i)))` (after `i-1` Gaussian elimination steps). Because `|D(i,i)| = 1`, no pivoting is needed for stability.
-   `A` is overwritten with the unit lower triangular factor `L` (below the diagonal) and the upper triangular factor `U` (on and above the diagonal).
-   This is the blocked right-looking driver. It calls the recursive kernel `zlaunhr_col_getrfnp2` on diagonal blocks and uses `ztrsm`/`zgemm` to update the trailing submatrix. The block size is hardcoded to `NB = 32` (matching LAPACK's `ILAENV` configuration for the related `LAORHR_COL_GETRFNP` family).
-   Used by the Householder reconstruction routine `zunhr_col`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zlaunhr_col_getrfnp = require( '@stdlib/lapack/base/zlaunhr_col_getrfnp' );

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp( 'column-major', 3, 3, A, 3, D, 1 );
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
