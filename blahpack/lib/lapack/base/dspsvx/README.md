# dspsvx

> Solve a real symmetric indefinite system A\*X = B (packed storage) with condition estimation and error bounds.

<section class="usage">

## Usage

```javascript
var dspsvx = require( '@stdlib/lapack/base/dspsvx' );
```

#### dspsvx.ndarray( fact, uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Uses the diagonal pivoting factorization A = U\*D\*U^T or A = L\*D\*L^T to solve a real symmetric system of linear equations A\*X = B, where A is an N-by-N symmetric matrix stored in packed format and X and B are N-by-NRHS matrices. Error bounds on the solution and a condition estimate are also provided.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 SPD matrix, upper packed: [4, 2, 5, 1, 3, 6]
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var AFP = new Float64Array( 6 );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
var X = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dspsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// X => ~[ 1.0, 1.0, 1.0 ]
```

The function has the following parameters:

-   **fact**: `'not-factored'` to compute the factorization, or `'factored'` if AFP and IPIV already contain it.
-   **uplo**: `'upper'` if upper triangle of A is packed, `'lower'` if lower triangle.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **AP**: symmetric matrix A in packed storage (length N\*(N+1)/2).
-   **strideAP**: stride for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: factored form of A in packed storage (output if fact='not-factored', input if fact='factored').
-   **strideAFP**: stride for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **IPIV**: pivot indices (output if fact='not-factored', input if fact='factored').
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix (column-major, N-by-NRHS).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (column-major, N-by-NRHS, output).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **rcond**: single-element `Float64Array` for reciprocal condition number (output).
-   **FERR**: forward error bounds array (length NRHS, output).
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds array (length NRHS, output).
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (length at least 3\*N).
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array (length at least N).
-   **strideIWORK**: stride for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine returns an integer `info`: 0 on success, `k > 0` if `D(k,k)` is exactly zero (singular factorization), or `N+1` if the reciprocal condition number is less than machine epsilon.
-   When `fact = 'not-factored'`, the routine copies AP to AFP and computes the factorization via `dsptrf`. When `fact = 'factored'`, the caller must supply a valid factorization in AFP and IPIV.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dspsvx = require( '@stdlib/lapack/base/dspsvx' );

// Solve A*x = b for a 3x3 symmetric matrix in lower packed storage:
var AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
var AFP = new Float64Array( 6 );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
var X = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dspsvx.ndarray( 'not-factored', 'lower', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );

console.log( 'info:', info );
console.log( 'X:', X );
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
