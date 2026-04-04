# zpbsvx

> Solves a complex system A * X = B where A is Hermitian positive definite band, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var zpbsvx = require( '@stdlib/lapack/base/zpbsvx' );
```

#### zpbsvx.ndarray( fact, uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, AFB, strideAFB1, strideAFB2, offsetAFB, equed, s, strideS, offsetS, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Solves a complex system A * X = B where A is Hermitian positive definite band, with equilibration, condition estimation, and error bounds.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Complex128Array( [ 0, 0, 4, 0, -1, 0.5, 4, 0, -1, 0.5, 4, 0 ] );
var AFB = new Complex128Array( 6 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0 ] );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zpbsvx.ndarray( 'not-factored', 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **fact**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **kd**: kd.
-   **nrhs**: nrhs.
-   **AB**: input matrix.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: input matrix.
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
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
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   AB, AFB, B, X, and WORK are `Complex128Array` with strides/offsets in complex elements.
-   S, FERR, BERR, RWORK, and rcond are `Float64Array`.
-   `equed` is a single-element array: `['none']` or `['yes']`.
-   `rcond` is a single-element `Float64Array` for the reciprocal condition number.
-   `fact` is `'not-factored'`, `'factored'`, or `'equilibrate'`.
-   `uplo` is `'upper'` or `'lower'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbsvx = require( '@stdlib/lapack/base/zpbsvx' );

var AB = new Complex128Array( [ 0, 0, 4, 0, -1, 0.5, 4, 0, -1, 0.5, 4, 0 ] );
var AFB = new Complex128Array( 6 );
var S = new Float64Array( 3 );
var equed = [ 'none' ];
var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0 ] );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zpbsvx.ndarray( 'not-factored', 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'X:', Array.from( reinterpret( X, 0 ) ) );
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
