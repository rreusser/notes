# zpftrs

> Solves a complex system A * X = B where A is Hermitian positive definite in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var zpftrs = require( '@stdlib/lapack/base/zpftrs' );
```

#### zpftrs.ndarray( transr, uplo, N, nrhs, A, strideA, offsetA, B, strideB1, strideB2, offsetB )

Solves a system of linear equations `A * X = B` with a Hermitian positive definite matrix `A` in RFP format, using the Cholesky factorization computed by `zpftrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );
zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );

// Right-hand side B (3x1):
var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
var info = zpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of matrix A.
-   **nrhs**: number of right-hand side columns.
-   **A**: [`Complex128Array`][@stdlib/array/complex128] containing the Cholesky factor in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **B**: [`Complex128Array`][@stdlib/array/complex128] containing the N-by-NRHS right-hand side matrix.
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrix `A` must contain the Cholesky factorization computed by [`zpftrf`][@stdlib/lapack/base/zpftrf] in RFP format.
-   If `uplo = 'lower'`, the routine solves `L * L^H * X = B` using forward and back substitution.
-   If `uplo = 'upper'`, the routine solves `U^H * U * X = B` using forward and back substitution.
-   The solution `X` overwrites `B` on exit.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );
var zpftrs = require( '@stdlib/lapack/base/zpftrs' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );
zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );

// Right-hand side B (3x1):
var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
var info = zpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
console.log( 'info:', info );
// => info: 0

console.log( 'Solution:', reinterpret( B, 0 ) );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/array/complex128
[@stdlib/lapack/base/zpftrf]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/lapack/base/zpftrf

</section>

<!-- /.links -->
