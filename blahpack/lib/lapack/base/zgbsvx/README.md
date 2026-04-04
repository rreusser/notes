# zgbsvx

> Expert driver for solving a complex system of linear equations A\*X = B where A is a general band matrix, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var zgbsvx = require( '@stdlib/lapack/base/zgbsvx' );
```

#### zgbsvx.ndarray( fact, trans, N, kl, ku, nrhs, AB, sAB1, sAB2, oAB, AFB, sAFB1, sAFB2, oAFB, IPIV, sIPIV, oIPIV, equed, r, sR, oR, c, sC, oC, B, sB1, sB2, oB, X, sX1, sX2, oX, FERR, sFERR, oFERR, BERR, sBERR, oBERR, WORK, sWORK, oWORK, RWORK, sRWORK, oRWORK )

Expert driver for solving a complex system of linear equations A\*X = B where A is a general band matrix with KL subdiagonals and KU superdiagonals.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var N = 3;
var KL = 1;
var KU = 1;
var nrhs = 1;
var nRows = KL + KU + 1;

var AB = new Complex128Array( [ 0,0, 1,0, 1,0, 4,0, 3,0, 2,0, 1,0, 1,0, 0,0 ] );
var AFB = new Complex128Array( ((2*KL)+KU+1) * N );
var IPIV = new Int32Array( N );
var r = new Float64Array( N );
var c = new Float64Array( N );
var B = new Complex128Array( [ 5,0, 5,0, 3,0 ] );
var X = new Complex128Array( N );
var FERR = new Float64Array( nrhs );
var BERR = new Float64Array( nrhs );
var WORK = new Complex128Array( 2*N );
var RWORK = new Float64Array( N );

var out = zgbsvx.ndarray( 'not-factored', 'no-transpose', N, KL, KU, nrhs, AB, 1, nRows, 0, AFB, 1, (2*KL)+KU+1, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, N, 0, X, 1, N, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// out.info => 0
```

The function has the following parameters:

-   **fact**: `'not-factored'`, `'equilibrate'`, or `'factored'`.
-   **trans**: `'no-transpose'`, `'transpose'`, or `'conjugate-transpose'`.
-   **N**: order of the matrix A (N >= 0).
-   **kl**: number of subdiagonals (kl >= 0).
-   **ku**: number of superdiagonals (ku >= 0).
-   **nrhs**: number of right-hand side columns.
-   **AB**: [`Complex128Array`][@stdlib/array/complex128] band matrix in band storage ((KL+KU+1) x N).
-   **sAB1**: stride of the first dimension of `AB`.
-   **sAB2**: stride of the second dimension of `AB`.
-   **oAB**: starting index for `AB`.
-   **AFB**: [`Complex128Array`][@stdlib/array/complex128] factored band matrix ((2\*KL+KU+1) x N, output).
-   **sAFB1**: stride of the first dimension of `AFB`.
-   **sAFB2**: stride of the second dimension of `AFB`.
-   **oAFB**: starting index for `AFB`.
-   **IPIV**: [`Int32Array`][@stdlib/array/int32] pivot indices (0-based, output).
-   **sIPIV**: stride for `IPIV`.
-   **oIPIV**: starting index for `IPIV`.
-   **equed**: equilibration type (`'none'`, `'row'`, `'column'`, `'both'`). Input if fact=`'factored'`.
-   **r**: [`Float64Array`][@stdlib/array/float64] row scale factors.
-   **sR**: stride for `r`.
-   **oR**: index offset for `r`.
-   **c**: [`Float64Array`][@stdlib/array/float64] column scale factors.
-   **sC**: stride for `c`.
-   **oC**: index offset for `c`.
-   **B**: [`Complex128Array`][@stdlib/array/complex128] N-by-NRHS right-hand side.
-   **sB1**: stride of the first dimension of `B`.
-   **sB2**: stride of the second dimension of `B`.
-   **oB**: starting index for `B`.
-   **X**: [`Complex128Array`][@stdlib/array/complex128] N-by-NRHS solution matrix (output).
-   **sX1**: stride of the first dimension of `X`.
-   **sX2**: stride of the second dimension of `X`.
-   **oX**: starting index for `X`.
-   **FERR**: [`Float64Array`][@stdlib/array/float64] forward error bounds (output).
-   **sFERR**: stride for `FERR`.
-   **oFERR**: starting index for `FERR`.
-   **BERR**: [`Float64Array`][@stdlib/array/float64] backward error bounds (output).
-   **sBERR**: stride for `BERR`.
-   **oBERR**: starting index for `BERR`.
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] workspace (length >= 2\*N).
-   **sWORK**: stride for `WORK`.
-   **oWORK**: starting index for `WORK`.
-   **RWORK**: [`Float64Array`][@stdlib/array/float64] real workspace (length >= N).
-   **sRWORK**: stride for `RWORK`.
-   **oRWORK**: starting index for `RWORK`.

The function returns an object `{ info, equed, rcond, rpvgrw }`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgbsvx` performs the following steps: (1) optionally equilibrate the matrix using `zgbequ`/`zlaqgb`, (2) compute the LU factorization via `zgbtrf`, (3) estimate the condition number via `zgbcon`, (4) solve the system via `zgbtrs`, (5) compute error bounds via `zgbrfs`, and (6) transform the solution back if equilibration was used.
-   When `info > 0` and `info <= N`, `U(info, info)` is exactly zero and the factorization is incomplete. When `info = N+1`, the matrix is nonsingular but ill-conditioned (rcond < machine epsilon).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgbsvx = require( '@stdlib/lapack/base/zgbsvx' );

var N = 3;
var KL = 1;
var KU = 1;
var nrhs = 1;
var nRows = KL + KU + 1;

var AB = new Complex128Array( [ 0,0, 1,0, 1,0, 4,0, 3,0, 2,0, 1,0, 1,0, 0,0 ] );
var AFB = new Complex128Array( ((2*KL)+KU+1) * N );
var IPIV = new Int32Array( N );
var r = new Float64Array( N );
var c = new Float64Array( N );
var B = new Complex128Array( [ 5,0, 5,0, 3,0 ] );
var X = new Complex128Array( N );
var FERR = new Float64Array( nrhs );
var BERR = new Float64Array( nrhs );
var WORK = new Complex128Array( 2*N );
var RWORK = new Float64Array( N );

var result = zgbsvx.ndarray( 'not-factored', 'no-transpose', N, KL, KU, nrhs, AB, 1, nRows, 0, AFB, 1, (2*KL)+KU+1, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, N, 0, X, 1, N, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// result.info => 0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib
[@stdlib/array/float64]: https://github.com/stdlib-js/stdlib
[@stdlib/array/int32]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
