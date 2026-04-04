# dtfsm

> Solves a matrix equation with a triangular matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var dtfsm = require( '@stdlib/lapack/base/dtfsm' );
```

#### dtfsm.ndarray( transr, side, uplo, trans, diag, M, N, alpha, A, strideA, offsetA, B, strideB1, strideB2, offsetB )

Solves one of the matrix equations `op(A)*X = alpha*B` or `X*op(A) = alpha*B` where `A` is a triangular matrix stored in Rectangular Full Packed (RFP) format.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 lower triangular in RFP format (TRANSR='N', UPLO='L'):
// [4, 0.5, 1.0, 6, 5, 1.5]
var A = new Float64Array( [ 4.0, 0.5, 1.0, 6.0, 5.0, 1.5 ] );

// 3x2 matrix B (column-major):
var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

// Solve A*X = B:
dtfsm.ndarray( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, 1.0, A, 1, 0, B, 1, 3, 0 );
// B is overwritten with X
```

The function has the following parameters:

-   **transr**: specifies the storage format of A (`'no-transpose'` or `'transpose'`).
-   **side**: specifies whether `op(A)` appears on the left or right (`'left'` or `'right'`).
-   **uplo**: specifies whether A is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the transpose operation on A (`'no-transpose'` or `'transpose'`).
-   **diag**: specifies whether A has unit diagonal (`'unit'` or `'non-unit'`).
-   **M**: number of rows of B.
-   **N**: number of columns of B.
-   **alpha**: scalar multiplier.
-   **A**: RFP array of length `K*(K+1)/2` where `K=M` (if `side='left'`) or `K=N` (if `side='right'`).
-   **strideA**: stride for `A`.
-   **offsetA**: starting index for `A`.
-   **B**: M-by-N input/output matrix (overwritten with X).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dtfsm` is a Level 3 BLAS-like operation for matrices stored in Rectangular Full Packed (RFP) format.
-   The RFP format stores a triangular matrix in a compact 1D array of length `N*(N+1)/2`. The `transr` parameter controls whether the RFP is in normal or transposed form.
-   When `alpha = 0`, B is set to the zero matrix regardless of A.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dtfsm = require( '@stdlib/lapack/base/dtfsm' );

// 3x3 lower triangular in RFP (TRANSR='N', UPLO='L'):
var A = new Float64Array( [ 4.0, 0.5, 1.0, 6.0, 5.0, 1.5 ] );

// 3x2 matrix B (column-major):
var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );

// Solve A*X = B with alpha=1:
dtfsm.ndarray( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 2, 1.0, A, 1, 0, B, 1, 3, 0 );

console.log( B );
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
