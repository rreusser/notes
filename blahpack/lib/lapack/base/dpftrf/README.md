# dpftrf

> Computes the Cholesky factorization of a real symmetric positive definite matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var dpftrf = require( '@stdlib/lapack/base/dpftrf' );
```

#### dpftrf.ndarray( transr, uplo, N, A, strideA, offsetA )

Computes the Cholesky factorization of a real symmetric positive definite matrix stored in RFP format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 10, 3, 1, 6, 8, 2 ] );
var info = dpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix.
-   **A**: input/output [`Float64Array`][mdn-float64array] in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The RFP (Rectangular Full Packed) format stores a triangular matrix in a rectangular array, enabling efficient use of Level 3 BLAS operations.
-   The factorization has the form `A = U^T * U` if `uplo = 'upper'`, or `A = L * L^T` if `uplo = 'lower'`.
-   On successful exit (`info = 0`), the factor `U` or `L` overwrites the input in RFP format.
-   If `info = k > 0`, the leading minor of order `k` is not positive definite and the factorization could not be completed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dpftrf = require( '@stdlib/lapack/base/dpftrf' );

// 3x3 SPD matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 10, 3, 1, 6, 8, 2 ] );
var info = dpftrf.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
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

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

</section>

<!-- /.links -->
