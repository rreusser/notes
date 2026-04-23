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

# dlaorhr_col_getrfnp

> Computes the modified LU factorization without pivoting of a general M-by-N matrix (blocked driver).

<section class="usage">

## Usage

```javascript
var dlaorhr_col_getrfnp = require( '@stdlib/lapack/base/dlaorhr_col_getrfnp' );
```

#### dlaorhr_col_getrfnp( order, M, N, A, LDA, d, strideD )

Computes the modified LU factorization without pivoting of a general M-by-N matrix (blocked driver).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 0.5, 0.3, -0.2, -0.4, 0.6, 0.1, 0.2, -0.1, 0.7 ] );
var D = new Float64Array( 3 );

dlaorhr_col_getrfnp( 'column-major', 3, 3, A, 3, D, 1 );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **d**: output array.
-   **strideD**: stride length for `d`.

#### dlaorhr_col_getrfnp.ndarray( M, N, A, strideA1, strideA2, offsetA, d, strideD, offsetD )

Computes the modified LU factorization without pivoting of a general M-by-N matrix (blocked driver)., using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 0.5, 0.3, -0.2, -0.4, 0.6, 0.1, 0.2, -0.1, 0.7 ] );
var D = new Float64Array( 3 );

dlaorhr_col_getrfnp.ndarray( 3, 3, A, 1, 3, 0, D, 1, 0 );
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

-   The factorization has the form `A - S = L * U` where `S` is an `M`-by-`N` diagonal sign matrix with diagonal `D`, `L` is `M`-by-`N` lower triangular with unit diagonal, and `U` is `M`-by-`N` upper triangular. The diagonal `D[i] = -sign(A[i,i])` is computed at each elimination step so that the pivot is at least one in absolute value, guaranteeing numerical stability without row interchanges.
-   This is the blocked driver. It delegates panel factorizations to `dlaorhr_col_getrfnp2` and uses `dtrsm`/`dgemm` for trailing updates. For matrices smaller than the block size (`NB = 32`), the routine falls back entirely to `dlaorhr_col_getrfnp2`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlaorhr_col_getrfnp = require( '@stdlib/lapack/base/dlaorhr_col_getrfnp' );

var A = new Float64Array( [ 0.5, 0.3, -0.2, -0.4, 0.6, 0.1, 0.2, -0.1, 0.7 ] );
var D = new Float64Array( 3 );

dlaorhr_col_getrfnp( 'column-major', 3, 3, A, 3, D, 1 );
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
