# zgtsvx

> Uses the LU factorization to compute the solution to a complex system of linear equations A\*X = B, A^T\*X = B, or A^H\*X = B, where A is a tridiagonal matrix. Provides condition estimation and error bounds.

<section class="usage">

## Usage

```javascript
var zgtsvx = require( '@stdlib/lapack/base/zgtsvx' );
```

#### zgtsvx.ndarray( fact, trans, N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DLF, strideDLF, offsetDLF, DF, strideDF, offsetDF, DUF, strideDUF, offsetDUF, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Expert driver for solving a complex tridiagonal system A\*X = B with condition estimation and iterative refinement.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var dl = new Complex128Array( new Float64Array( [ 1, 0.5, 2, -1 ] ) );
var d = new Complex128Array( new Float64Array( [ 4, 1, 5, 0, 3, -0.5 ] ) );
var du = new Complex128Array( new Float64Array( [ -1, 0.5, 1, 1 ] ) );
var dlf = new Complex128Array( 2 );
var df = new Complex128Array( 3 );
var duf = new Complex128Array( 2 );
var du2 = new Complex128Array( 1 );
var ipiv = new Int32Array( 3 );
var b = new Complex128Array( new Float64Array( [ 3, 1.5, 8, 0.5, 5, -1.5 ] ) );
var x = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Complex128Array( 6 );
var rwork = new Float64Array( 3 );

var info = zgtsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, 3, 0, x, 1, 3, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );
// info => 0
// rcond[0] => ~0.226
```

The function has the following parameters:

-   **fact**: `'not-factored'` to compute the factorization, or `'factored'` if DLF, DF, DUF, DU2, IPIV already contain the LU factorization.
-   **trans**: `'no-transpose'` (A\*X=B), `'transpose'` (A^T\*X=B), or `'conjugate-transpose'` (A^H\*X=B).
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **DL**: [`Complex128Array`][@stdlib/array/complex128] sub-diagonal of A (length N-1).
-   **strideDL**: stride for `DL` (in complex elements).
-   **offsetDL**: starting index for `DL` (in complex elements).
-   **d**: [`Complex128Array`][@stdlib/array/complex128] diagonal of A (length N).
-   **strideD**: stride for `d` (in complex elements).
-   **offsetD**: starting index for `d` (in complex elements).
-   **DU**: [`Complex128Array`][@stdlib/array/complex128] super-diagonal of A (length N-1).
-   **strideDU**: stride for `DU` (in complex elements).
-   **offsetDU**: starting index for `DU` (in complex elements).
-   **DLF**: [`Complex128Array`][@stdlib/array/complex128] factored sub-diagonal (length N-1).
-   **strideDLF**: stride for `DLF` (in complex elements).
-   **offsetDLF**: starting index for `DLF` (in complex elements).
-   **DF**: [`Complex128Array`][@stdlib/array/complex128] factored diagonal (length N).
-   **strideDF**: stride for `DF` (in complex elements).
-   **offsetDF**: starting index for `DF` (in complex elements).
-   **DUF**: [`Complex128Array`][@stdlib/array/complex128] factored super-diagonal (length N-1).
-   **strideDUF**: stride for `DUF` (in complex elements).
-   **offsetDUF**: starting index for `DUF` (in complex elements).
-   **DU2**: [`Complex128Array`][@stdlib/array/complex128] second superdiagonal fill-in (length N-2).
-   **strideDU2**: stride for `DU2` (in complex elements).
-   **offsetDU2**: starting index for `DU2` (in complex elements).
-   **IPIV**: [`Int32Array`][mdn-int32array] pivot indices (length N), 0-based.
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: [`Complex128Array`][@stdlib/array/complex128] right-hand side matrix (N x NRHS).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: [`Complex128Array`][@stdlib/array/complex128] solution matrix (N x NRHS), output.
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **rcond**: [`Float64Array`][mdn-float64array] single-element array; on output, contains the reciprocal condition number.
-   **FERR**: [`Float64Array`][mdn-float64array] forward error bounds (length NRHS), output.
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: [`Float64Array`][mdn-float64array] backward error bounds (length NRHS), output.
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] workspace (length at least 2\*N).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: [`Float64Array`][mdn-float64array] real workspace (length at least N).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

The function returns an integer `info`:

-   `0`: successful exit.
-   `> 0` and `<= N`: the factorization has a zero diagonal element at position `info`, so the matrix is singular.
-   `N + 1`: the reciprocal condition number is less than machine epsilon, indicating the matrix is singular to working precision.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   When `fact` is `'not-factored'`, the routine copies DL, D, DU into DLF, DF, DUF and computes the LU factorization via `zgttrf`. When `fact` is `'factored'`, DLF, DF, DUF, DU2, and IPIV must already contain a valid factorization.
-   The routine always computes error bounds (FERR, BERR) via iterative refinement (`zgtrfs`) and a condition estimate (rcond) via `zgtcon`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgtsvx = require( '@stdlib/lapack/base/zgtsvx' );

var dl = new Complex128Array( new Float64Array( [ 1, 0.5, 2, -1 ] ) );
var d = new Complex128Array( new Float64Array( [ 4, 1, 5, 0, 3, -0.5 ] ) );
var du = new Complex128Array( new Float64Array( [ -1, 0.5, 1, 1 ] ) );
var dlf = new Complex128Array( 2 );
var df = new Complex128Array( 3 );
var duf = new Complex128Array( 2 );
var du2 = new Complex128Array( 1 );
var ipiv = new Int32Array( 3 );
var b = new Complex128Array( new Float64Array( [ 3, 1.5, 8, 0.5, 5, -1.5 ] ) );
var x = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Complex128Array( 6 );
var rwork = new Float64Array( 3 );

var info = zgtsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, 3, 0, x, 1, 3, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );
console.log( 'info:', info );
console.log( 'rcond:', rcond[ 0 ] );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array

</section>

<!-- /.links -->
