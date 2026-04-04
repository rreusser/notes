# zppsv

> Compute the solution to a complex system of linear equations A\*X=B where A is Hermitian positive definite in packed storage.

<section class="usage">

## Usage

```javascript
var zppsv = require( '@stdlib/lapack/base/zppsv' );
```

#### zppsv( order, uplo, N, nrhs, AP, B, LDB )

Computes the solution to a complex system of linear equations `A * X = B`, where `A` is an N-by-N Hermitian positive definite matrix stored in packed format and `X` and `B` are N-by-NRHS matrices.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// 2x2 HPD packed upper: [ a11, a12, a22 ] = [ (4,0), (1,-2), (10,0) ]
var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -2.0, 10.0, 0.0 ] );
var B = new Complex128Array( [ 8.0, 4.0, 12.0, -6.0 ] );

var info = zppsv( 'column-major', 'upper', 2, 1, AP, B, 2 );
// returns 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies whether the upper or lower triangular part of `A` is packed (`'upper'` or `'lower'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand sides (columns of `B`).
-   **AP**: packed Hermitian positive definite matrix as a [`Complex128Array`][mdn-typed-array], length `N*(N+1)/2`.
-   **B**: right-hand side matrix as a [`Complex128Array`][mdn-typed-array], overwritten with the solution on exit.
-   **LDB**: leading dimension of `B` (column-major, `>= max(1,N)`).

#### zppsv.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB )

Computes the solution using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var AP = new Complex128Array( [ 10.0, 0.0, 3.0, -1.0, 8.0, 0.0, 1.0, 2.0, 2.0, -1.0, 6.0, 0.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

var info = zppsv.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
// returns 0
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses the Cholesky decomposition to factor `A` as `A = U^H*U` (if uplo is `'upper'`) or `A = L*L^H` (if uplo is `'lower'`), then solves the system using the factored form.
-   On exit, `AP` is overwritten with the Cholesky factor `U` or `L`.
-   If the return value is greater than zero, the leading principal minor of that order is not positive definite, so the factorization could not be completed and the solution has not been computed.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zppsv = require( '@stdlib/lapack/base/zppsv' );

// Solve a 3x3 Hermitian positive definite system A*X = B:
// A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
// Upper packed: [10, 3-i, 8, 1+2i, 2-i, 6]
var AP = new Complex128Array( [ 10.0, 0.0, 3.0, -1.0, 8.0, 0.0, 1.0, 2.0, 2.0, -1.0, 6.0, 0.0 ] );
var B = new Complex128Array( [ 1.0, 1.0, 2.0, -1.0, 3.0, 0.5 ] );

var info = zppsv.ndarray( 'upper', 3, 1, AP, 1, 0, B, 1, 3, 0 );
// info => 0
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
