# zpprfs

> Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite in packed storage and provides error bounds.

<section class="usage">

## Usage

```javascript
var zpprfs = require( '@stdlib/lapack/base/zpprfs' );
```

#### zpprfs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite in packed storage and provides error bounds.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Original 2x2 Hermitian positive definite matrix A (upper packed):
// A = [ 4+0i, 1-1i ]
//     [ 1+1i, 3+0i ]
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 3.0, 0.0 ] );

// Cholesky factor (upper, from zpptrf):
var AFP = new Complex128Array( [ 2.0, 0.0, 0.5, -0.5, 1.5811, 0.0 ] );

// Right-hand side B (1 column):
var B = new Complex128Array( [ 6.0, 2.0, 5.0, 3.0 ] );

// Initial solution X (e.g., from zpptrs):
var X = new Complex128Array( [ 1.0, 1.0, 1.0, 1.0 ] );

var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 2 );

zpprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// X is refined, FERR and BERR contain error bounds
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular factor is stored (`'upper'` or `'lower'`).
-   **N**: order of matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: original Hermitian positive definite matrix in packed storage (`Complex128Array`).
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **AFP**: Cholesky-factored matrix in packed storage from `zpptrf` (`Complex128Array`).
-   **strideAFP**: stride length for `AFP` (in complex elements).
-   **offsetAFP**: starting index for `AFP` (in complex elements).
-   **B**: right-hand side matrix (`Complex128Array`).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: solution matrix, refined on exit (`Complex128Array`).
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **FERR**: output forward error bounds, one per right-hand side (`Float64Array`).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds, one per right-hand side (`Float64Array`).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace array of length at least `2*N` (`Complex128Array`).
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace array of length at least `N` (`Float64Array`).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The original matrix `A` must have been previously factored by `zpptrf` as `A = U**H * U` (upper) or `A = L * L**H` (lower). The factored form is passed as `AFP`.
-   The routine iteratively refines the solution `X` and provides forward error bound (`FERR`) and componentwise relative backward error (`BERR`) for each right-hand side column.
-   Workspace arrays `WORK` (complex, length `>= 2*N`) and `RWORK` (real, length `>= N`) must be provided by the caller.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpprfs = require( '@stdlib/lapack/base/zpprfs' );

// Original 2x2 Hermitian positive definite matrix A (upper packed):
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 3.0, 0.0 ] );

// Cholesky factor (upper, from zpptrf):
var AFP = new Complex128Array( [ 2.0, 0.0, 0.5, -0.5, 1.5811, 0.0 ] );

// Right-hand side B (1 column):
var B = new Complex128Array( [ 6.0, 2.0, 5.0, 3.0 ] );

// Initial solution X (e.g., from zpptrs):
var X = new Complex128Array( [ 1.0, 1.0, 1.0, 1.0 ] );

var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 2 );

var info = zpprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
// => info: 0
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
