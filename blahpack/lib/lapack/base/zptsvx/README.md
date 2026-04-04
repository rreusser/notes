# zptsvx

> Solves a complex Hermitian positive definite tridiagonal system A\*X = B, with condition estimation and error bounds.

<section class="usage">

## Usage

```javascript
var zptsvx = require( '@stdlib/lapack/base/zptsvx' );
```

#### zptsvx.ndarray( fact, N, nrhs, d, strideD, offsetD, e, strideE, offsetE, DF, strideDF, offsetDF, EF, strideEF, offsetEF, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Solves a complex Hermitian positive definite tridiagonal system A\*X = B, with condition estimation and error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );

var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
var e = new Complex128Array( [ 1.0, 0.5, 0.5, -0.3, 0.2, 0.1 ] );
var df = new Float64Array( 4 );
var ef = new Complex128Array( 3 );
var b = new Complex128Array( [ 6.5, 4.0, 12.15, -4.55, 7.1, 3.25, -3.25, 7.0 ] );
var x = new Complex128Array( 4 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Complex128Array( 4 );
var rwork = new Float64Array( 4 );

var info = zptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **fact**: `'not-factored'` to compute factorization, `'factored'` if already factored.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **d**: diagonal elements of A ([`Float64Array`][mdn-float64array], length N, real, not modified).
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: off-diagonal elements of A (Complex128Array, length N-1, not modified).
-   **strideE**: stride length for `e` (in complex elements).
-   **offsetE**: starting index for `e` (in complex elements).
-   **DF**: factored diagonal ([`Float64Array`][mdn-float64array], length N).
-   **strideDF**: stride length for `DF`.
-   **offsetDF**: starting index for `DF`.
-   **EF**: factored off-diagonal (Complex128Array, length N-1).
-   **strideEF**: stride length for `EF` (in complex elements).
-   **offsetEF**: starting index for `EF` (in complex elements).
-   **B**: right-hand side matrix (Complex128Array, N-by-NRHS, column-major).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: solution matrix (Complex128Array, N-by-NRHS, column-major, output).
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **rcond**: single-element [`Float64Array`][mdn-float64array] for the reciprocal condition number (output).
-   **FERR**: forward error bounds ([`Float64Array`][mdn-float64array], length NRHS, output).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds ([`Float64Array`][mdn-float64array], length NRHS, output).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace array (Complex128Array, length at least N).
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace array ([`Float64Array`][mdn-float64array], length at least N).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Uses the L\_D\_L^H factorization computed by `zpttrf`.
-   If `fact` is `'not-factored'`, the routine factors the matrix and copies D to DF and E to EF.
-   If `fact` is `'factored'`, DF and EF must already contain the factorization from `zpttrf`.
-   Returns 0 on success, k > 0 if D(k) <= 0 during factorization, N+1 if rcond < machine epsilon.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zptsvx = require( '@stdlib/lapack/base/zptsvx' );

var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
var e = new Complex128Array( [ 1.0, 0.5, 0.5, -0.3, 0.2, 0.1 ] );
var df = new Float64Array( 4 );
var ef = new Complex128Array( 3 );
var b = new Complex128Array( [ 6.5, 4.0, 12.15, -4.55, 7.1, 3.25, -3.25, 7.0 ] );
var x = new Complex128Array( 4 );
var rcond = new Float64Array( 1 );
var ferr = new Float64Array( 1 );
var berr = new Float64Array( 1 );
var work = new Complex128Array( 4 );
var rwork = new Float64Array( 4 );

var info = zptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );

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

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
