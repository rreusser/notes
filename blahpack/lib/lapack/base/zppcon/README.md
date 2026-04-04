# zppcon

> Estimates the reciprocal condition number of a complex Hermitian positive definite matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zppcon = require( '@stdlib/lapack/base/zppcon' );
```

#### zppcon.ndarray( uplo, N, AP, strideAP, offsetAP, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Estimates the reciprocal condition number of a complex Hermitian positive definite matrix in packed storage, using the Cholesky factorization `A = U^H * U` or `A = L * L^H` computed by `zpptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 identity in upper packed format (already factored):
var AP = new Complex128Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 ] );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );
var rcond = new Float64Array( 1 );

var info = zppcon.ndarray( 'upper', 3, AP, 1, 0, 1.0, rcond, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// rcond[ 0 ] => 1.0
```

The function has the following parameters:

-   **uplo**: `'upper'` or `'lower'`, specifying whether upper or lower triangular factor is stored.
-   **N**: order of the matrix A (N >= 0).
-   **AP**: [`Complex128Array`][@stdlib/array/complex128] containing the packed Cholesky factor from `zpptrf`.
-   **strideAP**: stride for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **anorm**: the 1-norm of the original matrix A.
-   **rcond**: [`Float64Array`][mdn-float64array] of length 1; on exit, `rcond[0]` is the reciprocal condition number.
-   **WORK**: [`Complex128Array`][@stdlib/array/complex128] workspace of length `2*N`.
-   **strideWORK**: stride for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: [`Float64Array`][mdn-float64array] workspace of length `N`.
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrix A must have been previously factored by `zpptrf`. The `AP` array should contain the packed upper or lower Cholesky factor.
-   The `anorm` parameter should be the 1-norm of the original (unfactored) matrix A, which can be computed using `zlanhp`.
-   The routine returns `info = 0` on success. The reciprocal condition number is stored in `rcond[0]`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zppcon = require( '@stdlib/lapack/base/zppcon' );

// 3x3 HPD matrix factored by zpptrf, upper packed:
var AP = new Complex128Array( [
    2.0, 0.0,
    0.5, 0.5,
    1.58113883008418976, 0.0,
    0.0, 0.0,
    0.6324555320336759, 0.0,
    1.2649110640673518, 0.0
] );

var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var anorm = 5.41421356237309492;

var info = zppcon.ndarray( 'upper', 3, AP, 1, 0, anorm, rcond, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
// rcond[ 0 ] => ~0.1917
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
[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
