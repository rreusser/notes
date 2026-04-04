# dpprfs

> Improves the computed solution to a real system A * X = B where A is symmetric positive definite in packed storage and provides error bounds.

<section class="usage">

## Usage

```javascript
var dpprfs = require( '@stdlib/lapack/base/dpprfs' );
```

#### dpprfs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Improves the computed solution to a real system A * X = B where A is symmetric positive definite in packed storage and provides error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 SPD matrix in upper packed storage: [ 4 2 1; 2 5 3; 1 3 6 ]
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var AFP = new Float64Array( [ 2.0, 1.0, 2.0, 0.5, 1.25, 2.0 ] ); // factored
var B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var X = new Float64Array( [ 0.194, 0.0597, 0.1045 ] ); // initial solve
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpprfs.ndarray( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether `'upper'` or `'lower'` triangle is stored.
-   **N**: order of matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: original symmetric matrix in packed storage.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: Cholesky-factored matrix in packed storage (from `dpptrf`).
-   **strideAFP**: stride length for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **B**: right-hand side matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (improved on exit).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bounds (length `nrhs`).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds (length `nrhs`).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (length >= `3*N`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array (length >= `N`).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `AP` and `AFP` are packed symmetric matrices of length `N*(N+1)/2`.
-   `WORK` must have length at least `3*N` and `IWORK` at least `N`.
-   On exit, `FERR` contains componentwise forward error bounds and `BERR` contains componentwise backward error bounds for each right-hand side.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpprfs = require( '@stdlib/lapack/base/dpprfs' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var AFP = new Float64Array( [ 2.0, 1.0, 2.0, 0.5, 1.25, 2.0 ] );
var B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
var X = new Float64Array( [ 0.194, 0.0597, 0.1045 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpprfs.ndarray( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'X:', X );
console.log( 'FERR:', FERR );
console.log( 'BERR:', BERR );
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
