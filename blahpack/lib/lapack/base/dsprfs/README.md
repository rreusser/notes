# dsprfs

> Improves the computed solution to a real system A * X = B where A is symmetric in packed storage and provides error bounds and backward error estimates.

<section class="usage">

## Usage

```javascript
var dsprfs = require( '@stdlib/lapack/base/dsprfs' );
```

#### dsprfs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Improves the computed solution to a real system A * X = B where A is symmetric in packed storage and provides error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 symmetric matrix [4 2 1; 2 5 3; 1 3 6] in upper packed storage:
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var AFP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

// After factoring AFP with dsptrf and solving with dsptrs:
var info = dsprfs.ndarray( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: original symmetric packed matrix of length `N*(N+1)/2`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: factored packed matrix from `dsptrf`, length `N*(N+1)/2`.
-   **strideAFP**: stride length for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **IPIV**: pivot indices from `dsptrf` (0-based).
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
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
-   **WORK**: workspace array (allocated internally, length `3*N`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: workspace array (allocated internally, length `N`).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses iterative refinement to improve the computed solution and uses `dlacn2` to estimate componentwise relative backward error and forward error bounds.
-   `AP` and `AFP` are packed symmetric matrices stored in either upper or lower packed format. For upper, the elements are stored column by column: `A(1,1), A(1,2), A(2,2), A(1,3), ...`. For lower: `A(1,1), A(2,1), A(3,1), A(2,2), ...`.
-   `WORK` and `IWORK` workspace arrays are accepted in the signature for API compatibility but are allocated internally.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
var dsptrs = require( '@stdlib/lapack/base/dsptrs' );
var dsprfs = require( '@stdlib/lapack/base/dsprfs' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var AFP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

dsptrf.ndarray( 'upper', 3, AFP, 1, 0, IPIV, 1, 0 );
dsptrs.ndarray( 'upper', 3, 1, AFP, 1, 0, IPIV, 1, 0, X, 1, 3, 0 );
dsprfs.ndarray( 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
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
