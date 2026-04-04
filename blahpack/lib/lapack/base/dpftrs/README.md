# dpftrs

> Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var dpftrs = require( '@stdlib/lapack/base/dpftrs' );
```

#### dpftrs.ndarray( transr, uplo, N, nrhs, A, strideA, offsetA, B, strideB1, strideB2, offsetB )

Solves a system of linear equations `A * X = B` using the Cholesky factorization computed by `dpftrf`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Cholesky factor of a 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 3.162, 0.949, 0.316, 2.344, 2.665, 0.638 ] );
var B = new Float64Array( [ 33.0, 38.0, 32.0 ] );
var info = dpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'transpose'`).
-   **uplo**: specifies whether the upper or lower triangular factor is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **nrhs**: number of right-hand side columns.
-   **A**: [`Float64Array`][mdn-float64array] containing the Cholesky factor in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.
-   **B**: [`Float64Array`][mdn-float64array] containing the N-by-NRHS right-hand side matrix, overwritten with the solution on exit.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The input matrix `A` must contain the Cholesky factorization as computed by [`dpftrf`][@stdlib/lapack/base/dpftrf].
-   If `uplo = 'lower'`, the routine solves `L * L^T * X = B`. If `uplo = 'upper'`, it solves `U^T * U * X = B`.
-   On successful exit, the solution `X` overwrites `B`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpftrf = require( '@stdlib/lapack/base/dpftrf' );
var dpftrs = require( '@stdlib/lapack/base/dpftrs' );

// 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 10, 3, 1, 6, 8, 2 ] );
dpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );

var B = new Float64Array( [ 33.0, 38.0, 32.0 ] );
var info = dpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
console.log( 'info:', info );
// => info: 0

console.log( 'Solution:', B );
// => Solution: Float64Array [ ~2.0, ~3.0, ~4.0 ]
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
[@stdlib/lapack/base/dpftrf]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
