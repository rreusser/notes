# zhprfs

> Improve the computed solution to a complex Hermitian system `A * X = B` where `A` is in packed storage, and provide forward and backward error bounds.

<section class="usage">

## Usage

```javascript
var zhprfs = require( '@stdlib/lapack/base/zhprfs' );
```

#### zhprfs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Improves the computed solution to `A * X = B` where `A` is a complex Hermitian matrix in packed storage, and provides error bounds and backward error estimates. Uses the factorization `A = U*D*U**H` or `A = L*D*L**H` computed by `zhptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var AFP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
var X = new Complex128Array( [ 0.25, 0.0, 0.375, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 2 );

var info = zhprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle of `A` is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand side columns in `B` and `X`.
-   **AP**: original Hermitian matrix in packed storage as a [`Complex128Array`][mdn-typed-array].
-   **strideAP**: stride for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: factored packed matrix from `zhptrf` as a [`Complex128Array`][mdn-typed-array].
-   **strideAFP**: stride for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **IPIV**: pivot indices from `zhptrf` as an [`Int32Array`][mdn-int32array].
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix as a [`Complex128Array`][mdn-typed-array].
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (improved on exit) as a [`Complex128Array`][mdn-typed-array].
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bound for each right-hand side as a [`Float64Array`][mdn-float64array].
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bound for each right-hand side as a [`Float64Array`][mdn-float64array].
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array as a [`Complex128Array`][mdn-typed-array].
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: real workspace array as a [`Float64Array`][mdn-float64array].
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `AP` and `AFP` store the Hermitian matrix and its factorization in packed format (upper or lower triangle, column-major).
-   `WORK` should have length at least `2*N` and `RWORK` at least `N`.
-   `IPIV` uses zero-based indexing.
-   The routine performs iterative refinement using `zhpmv` for residual computation and `zhptrs` for correction, with `zlacn2` for condition number estimation.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zhprfs = require( '@stdlib/lapack/base/zhprfs' );

var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var AFP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
var X = new Complex128Array( [ 0.25, 0.0, 0.375, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );

var info = zhprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, new Complex128Array( 4 ), 1, 0, new Float64Array( 2 ), 1, 0 );
// info => 0
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
