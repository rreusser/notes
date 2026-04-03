# dspcon

> Estimates the reciprocal condition number of a real symmetric packed matrix.

<section class="usage">

## Usage

```javascript
var dspcon = require( '@stdlib/lapack/base/dspcon' );
```

#### dspcon.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, anorm, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Estimates the reciprocal of the condition number (in the 1-norm) of a real symmetric matrix `A` in packed storage, using the factorization A = U\_D\_U^T or A = L\_D\_L^T computed by `dsptrf`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );

// 3x3 SPD matrix in upper packed storage:
var AP = new Float64Array( [ 4.0, 1.0, 3.0, 1.0, 1.0, 2.0 ] );
var IPIV = new Int32Array( 3 );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 6 );
var IWORK = new Int32Array( 3 );

dsptrf.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0 );
var info = dspcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 6.0, rcond, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// rcond[ 0 ] => ~0.177
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of A is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix A.
-   **AP**: factored packed matrix from `dsptrf`, length `N*(N+1)/2`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: pivot indices from `dsptrf` (0-based), length `N`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **anorm**: the 1-norm of the original matrix A.
-   **rcond**: output [`Float64Array`][mdn-float64array] of length 1; `rcond[0]` receives the reciprocal condition number.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `2*N`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: workspace [`Int32Array`][mdn-int32array] of length at least `N`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine requires the matrix to have been previously factored by `dsptrf`. The `IPIV` array uses 0-based indexing conventions.
-   If the matrix is singular (a zero diagonal in D for a 1x1 pivot), `rcond` is set to `0.0`.
-   If `N` is `0`, `rcond` is set to `1.0`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '@stdlib/lapack/base/dsptrf' );
var dspcon = require( '@stdlib/lapack/base/dspcon' );

var AP = new Float64Array( [ 4.0, 1.0, 3.0, 1.0, 1.0, 2.0 ] );
var IPIV = new Int32Array( 3 );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 6 );
var IWORK = new Int32Array( 3 );

dsptrf.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0 );
var info = dspcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 6.0, rcond, WORK, 1, 0, IWORK, 1, 0 );

console.log( 'info:', info );
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
