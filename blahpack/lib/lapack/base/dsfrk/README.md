# dsfrk

> Performs a symmetric rank-k operation for a matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var dsfrk = require( '@stdlib/lapack/base/dsfrk' );
```

#### dsfrk.ndarray( transr, uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC, offsetC )

Performs a symmetric rank-k operation for a matrix in Rectangular Full Packed (RFP) format:

```
C := alpha*A*A^T + beta*C  or  C := alpha*A^T*A + beta*C
```

where alpha and beta are real scalars, C is an N-by-N symmetric matrix stored in RFP format, and A is an N-by-K (or K-by-N) real matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// N=3, K=2, C in RFP format (transr='no-transpose', uplo='lower'):
var C = new Float64Array( [ 2.0, 3.0, 4.0, 6.0, 4.0, 5.0 ] );
var A = new Float64Array( [ 1.0, 2.0, 3.0, 0.5, 1.5, 2.5 ] );

dsfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 1.0, C, 1, 0 );
// C is updated in-place
```

The function has the following parameters:

-   **transr**: specifies the storage format of the RFP matrix (`'no-transpose'` or `'transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **trans**: specifies the operation (`'no-transpose'` for C := alpha\*A\*A^T + beta\*C, or `'transpose'` for C := alpha\*A^T\*A + beta\*C).
-   **N**: order of the symmetric matrix C.
-   **K**: number of columns of A (if trans='no-transpose') or rows of A (if trans='transpose').
-   **alpha**: scalar multiplier for A\*A^T or A^T\*A.
-   **A**: input matrix as a [`Float64Array`][mdn-float64array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **beta**: scalar multiplier for C.
-   **C**: RFP array of length N\*(N+1)/2 as a [`Float64Array`][mdn-float64array].
-   **strideC**: stride length for `C`.
-   **offsetC**: starting index for `C`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dsfrk` delegates to `dsyrk` and `dgemm` on sub-blocks of the RFP array. The routine has 16 code paths corresponding to the 2x2x2x2 combinations of TRANSR, UPLO, TRANS, and N parity (odd/even).
-   The RFP format packs an N-by-N symmetric/triangular matrix into an N\*(N+1)/2 element vector. See LAPACK documentation for details.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsfrk = require( '@stdlib/lapack/base/dsfrk' );

// 3x2 matrix A (column-major, strideA1=1, strideA2=3):
var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

// C in RFP format, N=3, transr='no-transpose', uplo='lower':
var C = new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );

// C := 1.0 * A * A^T + 0.0 * C
dsfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 0 );

console.log( C );
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
