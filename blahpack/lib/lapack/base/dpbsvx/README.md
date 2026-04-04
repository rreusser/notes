# dpbsvx

> Solves a real system A * X = B where A is symmetric positive definite band, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var dpbsvx = require( '@stdlib/lapack/base/dpbsvx' );
```

#### dpbsvx.ndarray( fact, uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, AFB, strideAFB1, strideAFB2, offsetAFB, equed, s, strideS, offsetS, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Solves a real system A * X = B where A is symmetric positive definite band, with equilibration, condition estimation, and error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 symmetric positive definite band matrix with KD=2, lower band storage:
// A = [4 2 1; 2 5 3; 1 3 6]
var AB = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 0.0, 6.0, 0.0, 0.0 ] );
var AFB = new Float64Array( 9 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];

// Right-hand side: b = A * [1; 1; 1] = [7; 10; 10]
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
var X = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpbsvx.ndarray(
    'not-factored', 'lower', 3, 2, 1,
    AB, 1, 3, 0, AFB, 1, 3, 0, equed, S, 1, 0,
    B, 1, 3, 0, X, 1, 3, 0, rcond,
    FERR, 1, 0, BERR, 1, 0,
    WORK, 1, 0, IWORK, 1, 0
);
// info => 0
// X => [ 1.0, 1.0, 1.0 ]
```

The function has the following parameters:

-   **fact**: specifies the operation type (`'not-factored'`, `'factored'`, or `'equilibrate'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix A.
-   **kd**: number of superdiagonals (upper) or subdiagonals (lower).
-   **nrhs**: number of right-hand side columns.
-   **AB**: band matrix in band storage, (KD+1)-by-N.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: factored band matrix (output if not factored).
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
-   **equed**: single-element array for equilibration status (`['none']` or `['yes']`).
-   **s**: scaling factors array (length N).
-   **strideS**: stride length for `s`.
-   **offsetS**: starting index for `s`.
-   **B**: right-hand side matrix (column-major, N-by-NRHS).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (column-major, N-by-NRHS, output).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **rcond**: single-element Float64Array for reciprocal condition number (output).
-   **FERR**: forward error bounds array (length NRHS, output).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds array (length NRHS, output).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (length at least 3\*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array (length at least N).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The `fact` parameter accepts `'not-factored'`, `'factored'`, or `'equilibrate'`.
-   The `equed` parameter is a single-element array (`['none']` or `['yes']`) that is both input and output.
-   When `fact` is `'equilibrate'`, the routine may modify `AB`, `S`, and `B`.
-   Returns `info`: `0` on success, `k > 0` if the leading minor of order `k` is not positive definite, `N+1` if the reciprocal condition number is less than machine epsilon.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpbsvx = require( '@stdlib/lapack/base/dpbsvx' );

var AB = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 0.0, 6.0, 0.0, 0.0 ] );
var AFB = new Float64Array( 9 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
var X = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpbsvx.ndarray(
    'not-factored', 'lower', 3, 2, 1,
    AB, 1, 3, 0, AFB, 1, 3, 0, equed, S, 1, 0,
    B, 1, 3, 0, X, 1, 3, 0, rcond,
    FERR, 1, 0, BERR, 1, 0,
    WORK, 1, 0, IWORK, 1, 0
);
console.log( 'info:', info );
// => info: 0
console.log( 'X:', X );
// => X: Float64Array [ 1.0, 1.0, 1.0 ]
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
