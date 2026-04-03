# ztptrs

> Solve a triangular system of equations with a complex triangular matrix stored in packed format.

<section class="usage">

## Usage

```javascript
var ztptrs = require( '@stdlib/lapack/base/ztptrs' );
```

#### ztptrs.ndarray( uplo, trans, diag, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB )

Solves a triangular system of the form `A*X = B` or `A**H*X = B` where `A` is a complex triangular matrix of order `N` stored in packed format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 3x3 upper triangular packed matrix:
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 4.0, 1.0, 3.0, 0.0, 5.0, -1.0, 6.0, 2.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

ztptrs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0 );
// B is overwritten with the solution X
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the operation (`'no-transpose'` or `'conjugate-transpose'`).
-   **diag**: specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: packed triangular matrix `A` (`Complex128Array`).
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **B**: right-hand side matrix, overwritten with solution on exit (`Complex128Array`).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   A check is made to verify that `A` is nonsingular. If a zero diagonal element is found, the routine returns the 1-based index of the zero element and does not solve the system.
-   The routine overwrites `B` with the solution matrix `X`.
-   For `'no-transpose'`, the routine solves `A * X = B` by calling `ztpsv` for each right-hand side column.
-   For `'conjugate-transpose'`, the routine solves `A**H * X = B` by calling `ztpsv` for each right-hand side column.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var ztptrs = require( '@stdlib/lapack/base/ztptrs' );

var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 4.0, 1.0, 3.0, 0.0, 5.0, -1.0, 6.0, 2.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

var info = ztptrs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0 );
console.log( 'info:', info );
// => 0
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
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
