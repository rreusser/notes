# zppsvx

> Solves a complex system A * X = B where A is Hermitian positive definite in packed storage, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var zppsvx = require( '@stdlib/lapack/base/zppsvx' );
```

#### zppsvx.ndarray( fact, uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, equed, s, strideS, offsetS, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Solves a complex system A * X = B where A is Hermitian positive definite in packed storage, with equilibration, condition estimation, and error bounds.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 Hermitian positive definite matrix in upper packed storage:
// A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
var AP = new Complex128Array( new Float64Array( [ 10, 0, 3, -1, 8, 0, 1, 2, 2, -1, 6, 0 ] ) );
var AFP = new Complex128Array( 6 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Complex128Array( new Float64Array( [ 1, 1, 2, -1, 3, 0.5 ] ) );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zppsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **fact**: `'not-factored'`, `'factored'`, or `'equilibrate'`.
-   **uplo**: `'upper'` or `'lower'`.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **AP**: [`Complex128Array`][@stdlib/array/complex128] Hermitian positive definite matrix in packed storage.
-   **strideAP**: stride for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **AFP**: [`Complex128Array`][@stdlib/array/complex128] factored packed matrix.
-   **strideAFP**: stride for `AFP` (in complex elements).
-   **offsetAFP**: starting index for `AFP` (in complex elements).
-   **equed**: single-element array for equilibration status (`'none'` or `'yes'`).
-   **s**: [`Float64Array`][mdn-float64array] scaling factors (length N).
-   **strideS**: stride for `s`.
-   **offsetS**: starting index for `s`.
-   **B**: [`Complex128Array`][@stdlib/array/complex128] right-hand side matrix (column-major, N-by-NRHS).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: [`Complex128Array`][@stdlib/array/complex128] solution matrix (output).
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **rcond**: [`Float64Array`][mdn-float64array] single-element array for reciprocal condition number (output).
-   **FERR**: [`Float64Array`][mdn-float64array] forward error bounds (length NRHS, output).
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: [`Float64Array`][mdn-float64array] backward error bounds (length NRHS, output).
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] complex workspace (length at least 2\*N).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: [`Float64Array`][mdn-float64array] real workspace (length at least N).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   AP, AFP, B, X, and WORK are `Complex128Array` with strides and offsets in complex elements.
-   S, FERR, BERR, RWORK, and rcond are `Float64Array`.
-   `fact` must be `'not-factored'`, `'factored'`, or `'equilibrate'`.
-   `uplo` must be `'upper'` or `'lower'`.
-   `equed` is a single-element array containing `'none'` or `'yes'`.
-   `rcond` is a single-element `Float64Array` for the reciprocal condition number.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zppsvx = require( '@stdlib/lapack/base/zppsvx' );

var AP = new Complex128Array( new Float64Array( [ 10, 0, 3, -1, 8, 0, 1, 2, 2, -1, 6, 0 ] ) );
var AFP = new Complex128Array( 6 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Complex128Array( new Float64Array( [ 1, 1, 2, -1, 3, 0.5 ] ) );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zppsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

console.log( 'info:', info );
// => 0

console.log( 'X:', reinterpret( X, 0 ) );
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
[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
