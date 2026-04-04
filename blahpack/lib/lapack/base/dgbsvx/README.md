# dgbsvx

> Solves a real system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.

<section class="usage">

## Usage

```javascript
var dgbsvx = require( '@stdlib/lapack/base/dgbsvx' );
```

#### dgbsvx.ndarray( fact, trans, N, kl, ku, nrhs, AB, strideAB1, strideAB2, offsetAB, AFB, strideAFB1, strideAFB2, offsetAFB, IPIV, strideIPIV, offsetIPIV, equed, r, strideR, offsetR, c, strideC, offsetC, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Solves a real system A * X = B where A is general band, with equilibration, condition estimation, and error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 tridiagonal: A = [4 1 0; 1 3 1; 0 1 2], b = [5, 5, 3]
var AB = new Float64Array( [ 0, 1, 1, 4, 3, 2, 1, 1, 0 ] );
var AFB = new Float64Array( 12 );
var IPIV = new Int32Array( 3 );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var B = new Float64Array( [ 5, 5, 3 ] );
var X = new Float64Array( 3 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var out = dgbsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, 1, 1, AB, 1, 3, 0, AFB, 1, 4, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// out.info => 0
// X => [ 1.0, 1.0, 1.0 ]
```

The function has the following parameters:

-   **fact**: `'not-factored'`, `'equilibrate'`, or `'factored'`.
-   **trans**: `'no-transpose'` or `'transpose'`.
-   **N**: order of matrix A.
-   **kl**: number of subdiagonals.
-   **ku**: number of superdiagonals.
-   **nrhs**: number of right-hand side columns.
-   **AB**: band matrix in band storage (`Float64Array`).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: factored band matrix output (`Float64Array`).
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
-   **IPIV**: pivot indices output (`Int32Array`).
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **equed**: equilibration type (`'none'`, `'row'`, `'column'`, `'both'`).
-   **r**: row scale factors (`Float64Array`).
-   **strideR**: stride length for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: column scale factors (`Float64Array`).
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **B**: right-hand side matrix (`Float64Array`).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix output (`Float64Array`).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: forward error bounds output (`Float64Array`).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds output (`Float64Array`).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (`Float64Array`, length >= 3\*N).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace (`Int32Array`, length >= N).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

The function returns an object with properties `info`, `equed`, `rcond`, and `rpvgrw`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `WORK[0]` contains the reciprocal pivot growth factor on output.
-   IPIV uses 0-based indexing.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbsvx = require( '@stdlib/lapack/base/dgbsvx' );

var AB = new Float64Array( [ 0, 1, 1, 4, 3, 2, 1, 1, 0 ] );
var AFB = new Float64Array( 12 );
var IPIV = new Int32Array( 3 );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var B = new Float64Array( [ 5, 5, 3 ] );
var X = new Float64Array( 3 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var out = dgbsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, 1, 1, AB, 1, 3, 0, AFB, 1, 4, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
console.log( 'info:', out.info );
console.log( 'X:', X );
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
