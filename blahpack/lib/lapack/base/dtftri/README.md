# dtftri

> Computes the inverse of a real triangular matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var dtftri = require( '@stdlib/lapack/base/dtftri' );
```

#### dtftri.ndarray( transr, uplo, diag, N, A, strideA, offsetA )

Computes the inverse of a real triangular matrix stored in RFP format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 lower triangular matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 4, 1, 1.5, 9, 7, 2.5 ] );
var info = dtftri.ndarray( 'no-transpose', 'lower', 'non-unit', 3, A, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **diag**: specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix.
-   **A**: input/output [`Float64Array`][mdn-float64array] in RFP format of length `N*(N+1)/2`.
-   **strideA**: stride length for `A`.
-   **offsetA**: starting index for `A`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The RFP (Rectangular Full Packed) format stores a triangular matrix in a rectangular array, enabling efficient use of Level 3 BLAS operations.
-   On successful exit (`info = 0`), the inverse of the triangular matrix overwrites the input in RFP format.
-   If `info = k > 0`, `A(k,k)` is exactly zero and the matrix is singular; the inverse cannot be computed.
-   For unit triangular matrices (`diag = 'unit'`), the diagonal elements are not referenced and are assumed to be one.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtftri = require( '@stdlib/lapack/base/dtftri' );

// 3x3 lower triangular matrix in RFP format (TRANSR='no-transpose', UPLO='lower'):
var A = new Float64Array( [ 4, 1, 1.5, 9, 7, 2.5 ] );
var info = dtftri.ndarray( 'no-transpose', 'lower', 'non-unit', 3, A, 1, 0 );
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
