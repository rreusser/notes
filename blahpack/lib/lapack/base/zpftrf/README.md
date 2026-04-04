# zpftrf

> Computes the Cholesky factorization of a complex Hermitian positive definite matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );
```

#### zpftrf.ndarray( transr, uplo, N, A, strideA, offsetA )

Computes the Cholesky factorization of a complex Hermitian positive definite matrix stored in RFP format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );
var info = zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
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
-   The factorization has the form `A = U^H * U` if `uplo = 'upper'`, or `A = L * L^H` if `uplo = 'lower'`.
-   On successful exit (`info = 0`), the factor `U` or `L` overwrites the input in RFP format.
-   If `info = k > 0`, the leading minor of order `k` is not positive definite and the factorization could not be completed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zpftrf = require( '@stdlib/lapack/base/zpftrf' );

// 3x3 HPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Complex128Array( [ 10, 0, 3, -1, 1, 2, 6, 0, 8, 0, 2, -1 ] );
var info = zpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
console.log( 'info:', info );
// => info: 0
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
