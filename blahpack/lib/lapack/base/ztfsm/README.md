# ztfsm

> Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.

<section class="usage">

## Usage

```javascript
var ztfsm = require( '@stdlib/lapack/base/ztfsm' );
```

#### ztfsm( transr, side, uplo, trans, diag, M, N, alpha, A, B )

Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed (RFP) format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var alpha = new Complex128( 1.0, 0.0 );
var A = new Complex128Array( [ 4, 0, 2, 1, 3, 1.5, 9, 0, 7, 0, 5, 2.5 ] );
var B = new Complex128Array( [ 1, 0.3, 2, 0.6, 3, 0.9 ] );
ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 1, alpha, A, B );
```

The function has the following parameters:

-   **transr**: specifies the RFP storage format (`'no-transpose'` or `'conjugate-transpose'`).
-   **side**: specifies whether op(A) appears on the left or right (`'left'` or `'right'`).
-   **uplo**: specifies whether A is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the operation applied to A (`'no-transpose'` or `'conjugate-transpose'`).
-   **diag**: specifies whether A is unit triangular (`'unit'` or `'non-unit'`).
-   **M**: number of rows of B.
-   **N**: number of columns of B.
-   **alpha**: complex scalar multiplier (`Complex128`).
-   **A**: RFP array (`Complex128Array`).
-   **B**: M-by-N complex matrix, column-major, tightly packed (`Complex128Array`). Overwritten with the solution X on exit.

#### ztfsm.ndarray( transr, side, uplo, trans, diag, M, N, alpha, A, strideA, offsetA, B, strideB1, strideB2, offsetB )

Solves a matrix equation with a complex triangular matrix in RFP format using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var alpha = new Complex128( 1.0, 0.0 );
var A = new Complex128Array( [ 4, 0, 2, 1, 3, 1.5, 9, 0, 7, 0, 5, 2.5 ] );
var B = new Complex128Array( [ 1, 0.3, 2, 0.6, 3, 0.9 ] );
ztfsm.ndarray( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 1, alpha, A, 1, 0, B, 1, 3, 0 );
```

The function has the following additional parameters:

-   **strideA**: stride for `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   ZTFSM solves `op(A) * X = alpha * B` or `X * op(A) = alpha * B` where A is stored in Rectangular Full Packed (RFP) format.
-   The routine decomposes the RFP triangular matrix into sub-blocks and delegates to `ztrsm` and `zgemm`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztfsm = require( '@stdlib/lapack/base/ztfsm' );

var alpha = new Complex128( 1.0, 0.0 );
var A = new Complex128Array( [ 4, 0, 2, 1, 3, 1.5, 9, 0, 7, 0, 5, 2.5 ] );
var B = new Complex128Array( [ 1, 0.3, 2, 0.6, 3, 0.9 ] );
ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 1, alpha, A, B );
console.log( reinterpret( B, 0 ) );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

</section>

<!-- /.links -->
