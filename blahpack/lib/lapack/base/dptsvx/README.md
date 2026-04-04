# dptsvx

> Solves a real symmetric positive definite tridiagonal system A\_X = B, and provides an estimate of the condition number and error bounds on the solution.

<section class="usage">

## Usage

```javascript
var dptsvx = require( '@stdlib/lapack/base/dptsvx' );
```

#### dptsvx.ndarray( fact, N, nrhs, d, strideD, offsetD, e, strideE, offsetE, DF, strideDF, offsetDF, EF, strideEF, offsetEF, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK )

Solves a real symmetric positive definite tridiagonal system A\_X = B, and provides an estimate of the condition number and error bounds on the solution.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
var e = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var df = new Float64Array( 4 );
var ef = new Float64Array( 3 );
var b = new Float64Array( [ 5.0, 8.0, 11.0, 10.0 ] );
var x = new Float64Array( 4 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Float64Array( 8 );

var info = dptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0 );
// info => 0
// x => [ 1.0, 1.0, 1.0, 1.0 ]
```

The function has the following parameters:

-   **fact**: `'not-factored'` to compute the L\_D\_L^T factorization, or `'factored'` if DF and EF already contain the factorization from `dpttrf`.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **d**: diagonal elements of A (length N).
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: off-diagonal elements of A (length N-1).
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **DF**: factored diagonal elements (length N, output).
-   **strideDF**: stride length for `DF`.
-   **offsetDF**: starting index for `DF`.
-   **EF**: factored off-diagonal elements (length N-1, output).
-   **strideEF**: stride length for `EF`.
-   **offsetEF**: starting index for `EF`.
-   **B**: right-hand side matrix (N-by-NRHS, column-major).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (N-by-NRHS, column-major, output).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **rcond**: single-element `Float64Array` for reciprocal condition number (output).
-   **FERR**: forward error bounds (length NRHS, output).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds (length NRHS, output).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (length at least 2\*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

The function returns an integer `info`:

-   `0`: successful exit.
-   `k > 0`: the leading minor of order `k` is not positive definite, so the factorization could not be completed.
-   `N + 1`: the reciprocal condition number is less than machine epsilon, indicating the matrix is singular to working precision.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine optionally factors the tridiagonal matrix A using the L\_D\_L^T factorization computed by `dpttrf`, solves the system using `dpttrs`, and refines the solution using `dptrfs`.
-   The reciprocal condition number is computed via `dptcon`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dptsvx = require( '@stdlib/lapack/base/dptsvx' );

// Solve A*x = b where A is a 4x4 SPD tridiagonal matrix:
var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
var e = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var df = new Float64Array( 4 );
var ef = new Float64Array( 3 );
var b = new Float64Array( [ 5.0, 8.0, 11.0, 10.0 ] );
var x = new Float64Array( 4 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Float64Array( 8 );

var info = dptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0 );
// info => 0
// x => [ 1.0, 1.0, 1.0, 1.0 ]
// rcond[0] ~ 0.18
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
