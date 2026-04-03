# zspsv

> Computes the solution to a complex system of linear equations A * X = B where A is symmetric in packed storage.

<section class="usage">

## Usage

```javascript
var zspsv = require( '@stdlib/lapack/base/zspsv' );
```

#### zspsv.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Computes the solution to a complex system of linear equations `A * X = B` where `A` is an N-by-N complex symmetric matrix stored in packed format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 symmetric: A = [(3+1i) (1-1i); (1-1i) (4+2i)]
// Upper packed: A(1,1), A(1,2), A(2,2)
var AP = new Complex128Array( [ 3.0, 1.0, 1.0, -1.0, 4.0, 2.0 ] );
var IPIV = new Int32Array( 2 );
var B = new Complex128Array( [ 4.0, 0.0, 5.0, 1.0 ] );

var info = zspsv.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
// info => 0
// B now contains the solution X
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangle of `A` is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: packed symmetric matrix as a [`Complex128Array`][@stdlib/array/complex128] of length `N*(N+1)/2`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **IPIV**: pivot index output array as an [`Int32Array`][mdn-int32array] of length `N`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix as a [`Complex128Array`][@stdlib/array/complex128].
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses Bunch-Kaufman diagonal pivoting: `A = U * D * U^T` (upper) or `A = L * D * L^T` (lower).
-   This is for complex **symmetric** (not Hermitian) matrices. The transpose (not conjugate transpose) is used.
-   On exit, `AP` contains the factored form and `IPIV` contains the pivot indices (0-based).
-   Returns 0 on success, or `k > 0` if `D(k,k)` is exactly zero (matrix is singular).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zspsv = require( '@stdlib/lapack/base/zspsv' );

// Solve a 3x3 complex symmetric system (upper packed)
var AP = new Complex128Array([
    4.0, 1.0, 2.0, -1.0, 5.0, 0.5, 1.0, 2.0, 3.0, -1.0, 6.0, 1.0
]);
var IPIV = new Int32Array( 3 );
var B = new Complex128Array([ 7.0, 2.0, 10.0, -1.5, 10.0, 2.0 ]);

var info = zspsv.ndarray( 'upper', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
// info => 0

var view = reinterpret( B, 0 );
// view => ~[ 1.0, 0.0, 1.0, 0.0, 1.0, 0.0 ]
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128
[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
