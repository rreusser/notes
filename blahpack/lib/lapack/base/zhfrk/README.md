# zhfrk

> Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var zhfrk = require( '@stdlib/lapack/base/zhfrk' );
```

#### zhfrk.ndarray( transr, uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, beta, C, strideC, offsetC )

Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed (RFP) format:

```
C := alpha*A*A^H + beta*C  or  C := alpha*A^H*A + beta*C
```

where alpha and beta are real scalars, C is an N-by-N Hermitian matrix stored in RFP format, and A is an N-by-K (or K-by-N) complex matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// N=3, K=2, C in RFP format (transr='no-transpose', uplo='lower'):
var C = new Complex128Array( 6 );
var A = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 1.0, 1.0, 2.0, 1.0, 3.0, 1.0 ] );

zhfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 0 );
// C is updated in-place
```

The function has the following parameters:

-   **transr**: specifies the storage format of the RFP matrix (`'no-transpose'` or `'conjugate-transpose'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **trans**: specifies the operation (`'no-transpose'` for C := alpha\*A\*A^H + beta\*C, or `'conjugate-transpose'` for C := alpha\*A^H\*A + beta\*C).
-   **N**: order of the Hermitian matrix C.
-   **K**: number of columns of A (if trans='no-transpose') or rows of A (if trans='conjugate-transpose').
-   **alpha**: real scalar multiplier for A\*A^H or A^H\*A.
-   **A**: complex input matrix as a [`Complex128Array`][mdn-typed-array].
-   **strideA1**: stride of the first dimension of `A` (in complex elements).
-   **strideA2**: stride of the second dimension of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **beta**: real scalar multiplier for C.
-   **C**: RFP array of length N\*(N+1)/2 as a [`Complex128Array`][mdn-typed-array].
-   **strideC**: stride length for `C` (in complex elements).
-   **offsetC**: starting index for `C` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhfrk` delegates to `zherk` and `zgemm` on sub-blocks of the RFP array. The routine has 16 code paths corresponding to the 2x2x2x2 combinations of TRANSR, UPLO, TRANS, and N parity (odd/even).
-   The RFP format packs an N-by-N Hermitian/triangular matrix into an N\*(N+1)/2 element vector. See LAPACK documentation for details.
-   Alpha and beta are real (not complex) scalars, matching the Fortran ZHFRK interface.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhfrk = require( '@stdlib/lapack/base/zhfrk' );

// 3x2 complex matrix A (column-major, strideA1=1, strideA2=3):
var A = new Complex128Array( [ 1.0, 0.5, 2.0, 0.5, 3.0, 0.5, 1.0, 1.0, 2.0, 1.0, 3.0, 1.0 ] );

// C in RFP format, N=3, transr='no-transpose', uplo='lower' (6 complex elements):
var C = new Complex128Array( 6 );

// C := 1.0 * A * A^H + 0.0 * C
zhfrk.ndarray( 'no-transpose', 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 0 );

console.log( reinterpret( C, 0 ) );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
