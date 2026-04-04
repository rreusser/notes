# dppcon

> Estimates the reciprocal condition number of a real symmetric positive definite matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dppcon = require( '@stdlib/lapack/base/dppcon' );
```

#### dppcon( uplo, N, AP, anorm, rcond, WORK, IWORK )

Estimates the reciprocal condition number of a real symmetric positive definite matrix in packed storage using the Cholesky factorization `A = U^T*U` or `A = L*L^T` computed by `dpptrf`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 identity in upper packed (already factored):
var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dppcon( 'upper', 3, AP, 1.0, rcond, WORK, IWORK );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular Cholesky factor is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **AP**: the Cholesky factorization from `dpptrf`, stored in packed form as a [`Float64Array`][mdn-float64array] of length `N*(N+1)/2`.
-   **anorm**: the 1-norm (or infinity-norm) of the original matrix `A`.
-   **rcond**: output [`Float64Array`][mdn-float64array] of length 1; on exit, `rcond[0]` contains the reciprocal condition number.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `3*N`.
-   **IWORK**: workspace [`Int32Array`][mdn-int32array] of length at least `N`.

#### dppcon.ndarray( uplo, N, AP, strideAP, offsetAP, anorm, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Estimates the reciprocal condition number with alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dppcon.ndarray( 'upper', 3, AP, 1, 0, 1.0, rcond, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The input array `AP` must contain the Cholesky factorization as computed by `dpptrf`. The original matrix is not passed to this routine.
-   The routine uses `dlatps` and `dlacn2` internally to estimate `norm(inv(A))` via reverse communication. The reciprocal condition number is then `rcond = 1 / (anorm * norm(inv(A)))`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpptrf = require( '@stdlib/lapack/base/dpptrf' );
var dppcon = require( '@stdlib/lapack/base/dppcon' );

// 3x3 SPD matrix A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
// Upper packed: [4, 1, 3, 1, 1, 2]
var AP = new Float64Array( [ 4.0, 1.0, 3.0, 1.0, 1.0, 2.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

// Factorize:
dpptrf( 'upper', 3, AP );

// Estimate condition number (anorm = 6.0):
var info = dppcon( 'upper', 3, AP, 6.0, rcond, WORK, IWORK );
console.log( 'info:', info );
console.log( 'rcond:', rcond[ 0 ] );
// => rcond ~ 0.177
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
