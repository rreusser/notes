# dspsv

> Compute the solution to a real system of linear equations A\*X=B where A is symmetric in packed storage.

<section class="usage">

## Usage

```javascript
var dspsv = require( '@stdlib/lapack/base/dspsv' );
```

#### dspsv( uplo, N, nrhs, AP, IPIV, B, LDB )

Computes the solution to a real system of linear equations `A * X = B`, where `A` is an N-by-N symmetric matrix stored in packed format and `X` and `B` are N-by-NRHS matrices.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 2x2 symmetric packed upper: [ a11, a12, a22 ]
var AP = new Float64Array( [ 4.0, 2.0, 5.0 ] );
var IPIV = new Int32Array( 2 );
var B = new Float64Array( [ 8.0, 12.0 ] );

var info = dspsv( 'upper', 2, 1, AP, IPIV, B, 2 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand sides (columns of `B`).
-   **AP**: packed symmetric matrix as a [`Float64Array`][mdn-float64array], length `N*(N+1)/2`.
-   **IPIV**: output pivot index array as an [`Int32Array`][mdn-int32array], length `N`.
-   **B**: right-hand side matrix as a [`Float64Array`][mdn-float64array], overwritten with the solution on exit.
-   **LDB**: leading dimension of `B` (column-major, `>= max(1,N)`).

#### dspsv.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB )

Computes the solution using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );

var info = dspsv.ndarray( 'upper', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
// returns 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses the Bunch-Kaufman diagonal pivoting method to factor `A` as `A = U*D*U^T` (if uplo is `'upper'`) or `A = L*D*L^T` (if uplo is `'lower'`), then solves the system using the factored form.
-   On exit, `AP` is overwritten with the block diagonal matrix `D` and the multipliers used to obtain `U` or `L`.
-   If the return value is greater than zero, `D(k,k)` is exactly zero and the factorization could not be used to solve the system.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dspsv = require( '@stdlib/lapack/base/dspsv' );

// Solve a 3x3 symmetric system A*x = b using upper packed storage:
// A = [ 4  2  1; 2  5  3; 1  3  6 ]
// b = A * [1,1,1]^T = [7, 10, 10]
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var IPIV = new Int32Array( 3 );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );

var info = dspsv( 'upper', 3, 1, AP, IPIV, B, 3 );
// info => 0, B => ~[1, 1, 1]
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
