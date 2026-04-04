# zhpsvx

> Solve a complex Hermitian indefinite system A\*X = B (packed storage) with condition estimation and error bounds.

<section class="usage">

## Usage

```javascript
var zhpsvx = require( '@stdlib/lapack/base/zhpsvx' );
```

#### zhpsvx.ndarray( fact, uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, rcond, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Uses the diagonal pivoting factorization A = U\*D\*U^H or A = L\*D\*L^H to solve a complex Hermitian system of linear equations A\*X = B, where A is an N-by-N Hermitian matrix stored in packed format and X and B are N-by-NRHS matrices. Error bounds on the solution and a condition estimate are also provided.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 Hermitian matrix, upper packed
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 5.0, 0.0, 2.0, -1.0, 3.0, 1.0, 6.0, 0.0 ] );
var AFP = new Complex128Array( 6 );
var IPIV = new Int32Array( 3 );
var B = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0, 1.0, -1.0 ] );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zhpsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **fact**: `'not-factored'` to compute the factorization, or `'factored'` if AFP and IPIV already contain it.
-   **uplo**: `'upper'` if upper triangle of A is packed, `'lower'` if lower triangle.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **AP**: Hermitian matrix A in packed storage (`Complex128Array`, length N\*(N+1)/2 complex elements).
-   **strideAP**: stride for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **AFP**: factored form of A in packed storage (`Complex128Array`, output if fact='not-factored', input if fact='factored').
-   **strideAFP**: stride for `AFP` (in complex elements).
-   **offsetAFP**: starting index for `AFP` (in complex elements).
-   **IPIV**: pivot indices (`Int32Array`, output if fact='not-factored', input if fact='factored').
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix (`Complex128Array`, column-major, N-by-NRHS).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: solution matrix (`Complex128Array`, column-major, N-by-NRHS, output).
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **rcond**: single-element `Float64Array` for reciprocal condition number (output).
-   **FERR**: forward error bounds array (`Float64Array`, length NRHS, output).
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: backward error bounds array (`Float64Array`, length NRHS, output).
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace array (`Complex128Array`, length at least 2\*N).
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace array (`Float64Array`, length at least N).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine returns an integer `info`: 0 on success, `k > 0` if `D(k,k)` is exactly zero (singular factorization), or `N+1` if the reciprocal condition number is less than machine epsilon.
-   When `fact = 'not-factored'`, the routine copies AP to AFP and computes the factorization via `zhptrf`. When `fact = 'factored'`, the caller must supply a valid factorization in AFP and IPIV.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpsvx = require( '@stdlib/lapack/base/zhpsvx' );

// 3x3 Hermitian matrix in upper packed storage
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, 2.0, 5.0, 0.0, 2.0, -1.0, 3.0, 1.0, 6.0, 0.0 ] );
var AFP = new Complex128Array( 6 );
var IPIV = new Int32Array( 3 );
var B = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0, 1.0, -1.0 ] );
var X = new Complex128Array( 3 );
var rcond = new Float64Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zhpsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

console.log( 'info:', info );
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
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
