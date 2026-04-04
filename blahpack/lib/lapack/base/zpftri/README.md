# zpftri

> Computes the inverse of a complex Hermitian positive definite matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var zpftri = require( '@stdlib/lapack/base/zpftri' );
```

#### zpftri.ndarray( transr, uplo, N, A, strideA, offsetA )

Computes the inverse of a complex Hermitian positive definite matrix stored in RFP format, using the Cholesky factorization computed by `zpftrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );

// First factorize:
zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );

// Then invert:
var info = zpftri.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **A**: input/output [`Complex128Array`][@stdlib/array/complex128] in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The RFP (Rectangular Full Packed) format stores a triangular matrix in a rectangular array, enabling efficient use of Level 3 BLAS operations.
-   The input matrix `A` must contain the Cholesky factorization as computed by `zpftrf`. On exit, `A` is overwritten with the inverse in the same RFP format.
-   If `info = k > 0`, the `(k,k)` element of the Cholesky factor is zero and the inverse could not be computed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );
var zpftri = require( '@stdlib/lapack/base/zpftri' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );

// Factorize, then invert:
zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
var info = zpftri.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
console.log( 'info:', info );
// => info: 0
console.log( 'A (inverse in RFP):', reinterpret( A, 0 ) );
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

</section>

<!-- /.links -->
