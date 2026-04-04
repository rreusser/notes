# dppsvx

> Solves a real system A * X = B where A is symmetric positive definite in packed storage, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var dppsvx = require( '@stdlib/lapack/base/dppsvx' );
```

#### dppsvx.ndarray( fact, uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, equed, s, strideS, offsetS, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Solves a real system A * X = B where A is symmetric positive definite in packed storage, with equilibration, condition estimation, and error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 symmetric positive definite matrix in lower packed storage:
// A = [4 2 1; 2 5 3; 1 3 6]
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var AFP = new Float64Array( 6 );
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

var info = dppsvx.ndarray(
    'not-factored', 'lower', 3, 1,
    AP, 1, 0, AFP, 1, 0, equed, S, 1, 0,
    B, 1, 3, 0, X, 1, 3, 0, rcond,
    FERR, 1, 0, BERR, 1, 0,
    WORK, 1, 0, IWORK, 1, 0
);
// info => 0
// X => [ 1.0, 1.0, 1.0 ]
```

The function has the following parameters:

-   **fact**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **nrhs**: nrhs.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: input array.
-   **strideAFP**: stride length for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **equed**: specifies the operation type.
-   **s**: input array.
-   **strideS**: stride length for `s`.
-   **offsetS**: starting index for `s`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: input matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **rcond**: rcond.
-   **FERR**: input array.
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: input array.
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The `fact` parameter accepts `'not-factored'`, `'factored'`, or `'equilibrate'`.
-   The `equed` parameter is a single-element array (`['none']` or `['yes']`) that is both input and output.
-   When `fact` is `'equilibrate'`, the routine may modify `AP`, `S`, and `B`.
-   Returns `info`: `0` on success, `k > 0` if the leading minor of order `k` is not positive definite, `N+1` if the reciprocal condition number is less than machine epsilon.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dppsvx = require( '@stdlib/lapack/base/dppsvx' );

var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var AFP = new Float64Array( 6 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
var X = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dppsvx.ndarray(
    'not-factored', 'lower', 3, 1,
    AP, 1, 0, AFP, 1, 0, equed, S, 1, 0,
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
