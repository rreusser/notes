# dppsv

> Compute the solution to a real system of linear equations A\*X=B where A is symmetric positive definite in packed storage.

<section class="usage">

## Usage

```javascript
var dppsv = require( '@stdlib/lapack/base/dppsv' );
```

#### dppsv( uplo, N, nrhs, AP, B, LDB )

Computes the solution to a real system of linear equations `A * X = B`, where `A` is an N-by-N symmetric positive definite matrix stored in packed format and `X` and `B` are N-by-NRHS matrices.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 2x2 symmetric positive definite packed upper: [ a11, a12, a22 ]
var AP = new Float64Array( [ 4.0, 2.0, 5.0 ] );
var B = new Float64Array( [ 8.0, 12.0 ] );

var info = dppsv( 'upper', 2, 1, AP, B, 2 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand sides (columns of `B`).
-   **AP**: packed symmetric positive definite matrix as a [`Float64Array`][mdn-float64array], length `N*(N+1)/2`.
-   **B**: right-hand side matrix as a [`Float64Array`][mdn-float64array], overwritten with the solution on exit.
-   **LDB**: leading dimension of `B` (column-major, `>= max(1,N)`).

#### dppsv.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB )

Computes the solution using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );

var info = dppsv.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
// returns 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses the Cholesky decomposition to factor `A` as `A = U^T*U` (if uplo is `'upper'`) or `A = L*L^T` (if uplo is `'lower'`), then solves the system using the factored form.
-   On exit, `AP` is overwritten with the Cholesky factor `U` or `L`.
-   If the return value is greater than zero, the leading principal minor of that order is not positive definite, so the factorization could not be completed and the solution has not been computed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dppsv = require( '@stdlib/lapack/base/dppsv' );

// Solve a 3x3 symmetric positive definite system A*x = b using upper packed storage:
// A = [ 4  2  1; 2  5  3; 1  3  6 ]
// b = A * [1,1,1]^T = [7, 10, 10]
var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );

var info = dppsv( 'upper', 3, 1, AP, B, 3 );
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
