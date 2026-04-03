# ztbtrs

> Solve a triangular banded system of equations with a complex triangular band matrix.

<section class="usage">

## Usage

```javascript
var ztbtrs = require( '@stdlib/lapack/base/ztbtrs' );
```

#### ztbtrs( order, uplo, trans, diag, N, kd, nrhs, AB, LDAB, B, LDB )

Solves a triangular banded system of the form `A*X = B` or `A**H*X = B` where A is a complex triangular band matrix of order N with kd super- or sub-diagonals, and B is an N-by-NRHS matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 0, 2, 0, 5, 0 ] );
var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );

var info = ztbtrs( 'column-major', 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 2, B, 3 );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies whether A is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the form of the system (`'no-transpose'` or `'conjugate-transpose'`).
-   **diag**: specifies whether A is unit or non-unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix A.
-   **kd**: number of super- or sub-diagonals of A.
-   **nrhs**: number of right-hand side columns.
-   **AB**: band matrix A in band storage as a [`Complex128Array`][@stdlib/array/complex128].
-   **LDAB**: leading dimension of `AB` (must be at least `kd+1`).
-   **B**: right-hand side matrix, overwritten with the solution on exit, as a [`Complex128Array`][@stdlib/array/complex128].
-   **LDB**: leading dimension of `B` (must be at least `max(1, N)`).

#### ztbtrs.ndarray( uplo, trans, diag, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB )

Solves a triangular banded system using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 0, 2, 0, 5, 0 ] );
var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );

var info = ztbtrs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 1, 2, 0, B, 1, 3, 0 );
// info => 0
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   A check is made to verify that A is nonsingular. If a zero is found on the diagonal (for non-unit triangular matrices), the function returns the 1-based index of the zero diagonal element without solving the system.
-   The matrix A is stored in band format. For upper triangular matrices, the diagonal is stored in the last row of the band array. For lower triangular matrices, the diagonal is stored in the first row.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztbtrs = require( '@stdlib/lapack/base/ztbtrs' );

// Upper triangular band matrix with KD=1, N=3:
var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 0, 2, 0, 5, 0 ] );
var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );

var info = ztbtrs( 'column-major', 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 2, B, 3 );
// info => 0
// B now contains the solution
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/array/complex128

</section>

<!-- /.links -->
