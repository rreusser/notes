# ztbrfs

> Provides error bounds for the solution to a system with a complex triangular band matrix.

<section class="usage">

## Usage

```javascript
var ztbrfs = require( '@stdlib/lapack/base/ztbrfs' );
```

#### ztbrfs.ndarray( uplo, trans, diag, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Provides error bounds for the solution to a system with a complex triangular band matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Upper triangular band matrix, N=3, KD=1, NRHS=1:
var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 1, 2, -1, 5, -1 ] );
var B = new Complex128Array( [ 4, 1, 6, 0, 5, -1 ] );
var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] ); // pre-computed solution
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztbrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 1, 2, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// BERR[ 0 ] => 0.0
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **diag**: specifies the operation type.
-   **N**: number of columns.
-   **kd**: kd.
-   **nrhs**: nrhs.
-   **AB**: input matrix.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: input matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: input array.
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: input array.
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   WORK must be a `Complex128Array` of length at least `2*N`.
-   RWORK must be a `Float64Array` of length at least `N`.
-   The solution matrix X must be computed before calling this routine (e.g., by `ztbtrs`).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbrfs = require( '@stdlib/lapack/base/ztbrfs' );

// Upper triangular band matrix, N=3, KD=1:
var AB = new Complex128Array( [ 0, 0, 3, 0, 1, 1, 4, 1, 2, -1, 5, -1 ] );
var B = new Complex128Array( [ 4, 1, 6, 0, 5, -1 ] );
var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztbrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, 1, AB, 1, 2, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'FERR:', FERR[ 0 ] );
console.log( 'BERR:', BERR[ 0 ] );
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
