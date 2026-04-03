# zpptrs

> Solve a system of linear equations with a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.

<section class="usage">

## Usage

```javascript
var zpptrs = require( '@stdlib/lapack/base/zpptrs' );
```

#### zpptrs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB )

Solves `A * X = B` where `A` is a complex Hermitian positive definite matrix in packed storage, using the Cholesky factorization `A = U**H * U` or `A = L * L**H` computed by `zpptrf`.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// Pre-factored upper Cholesky of a 3x3 Hermitian positive definite matrix:
var AP = new Complex128Array( [ 3.162, 0.0, 0.949, -0.316, 2.646, 0.0, 0.316, 0.632, 0.718, -0.643, 2.138, 0.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

zpptrs.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
// B is overwritten with the solution X
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular factor is stored (`'upper'` or `'lower'`).
-   **N**: order of matrix `A`.
-   **nrhs**: number of right-hand side columns.
-   **AP**: packed triangular factor from `zpptrf` (`Complex128Array`).
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

-   The matrix `A` must have been previously factored by `zpptrf` as `A = U**H * U` (upper) or `A = L * L**H` (lower).
-   The routine overwrites `B` with the solution matrix `X`.
-   For upper triangular storage, the routine solves `U**H * Y = B` then `U * X = Y`.
-   For lower triangular storage, the routine solves `L * Y = B` then `L**H * X = Y`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zpptrs = require( '@stdlib/lapack/base/zpptrs' );

var AP = new Complex128Array( [ 3.162, 0.0, 0.949, -0.316, 2.646, 0.0, 0.316, 0.632, 0.718, -0.643, 2.138, 0.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

var info = zpptrs.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
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
