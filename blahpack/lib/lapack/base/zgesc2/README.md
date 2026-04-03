# zgesc2

> Solves a system of linear equations using the LU factorization with complete pivoting computed by zgetc2.

<section class="usage">

## Usage

```javascript
var zgesc2 = require( '@stdlib/lapack/base/zgesc2' );
```

#### zgesc2.ndarray( N, A, strideA1, strideA2, offsetA, RHS, strideRHS, offsetRHS, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV, scale )

Solves a system of linear equations `A * X = scale * RHS` with a general N-by-N complex matrix A using the LU factorization with complete pivoting computed by [`zgetc2`][@stdlib/lapack/base/zgetc2].

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// A 2x2 LU-factored complex matrix (column-major):
var A = new Complex128Array( [ 4.0, 1.0, 0.353, 0.412, 2.0, -1.0, 1.882, 0.029 ] );
var RHS = new Complex128Array( [ 10.0, 3.0, 7.0, 4.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var JPIV = new Int32Array( [ 0, 1 ] );
var scale = new Float64Array( 1 );

zgesc2.ndarray( 2, A, 1, 2, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );
// RHS is overwritten with the solution, scale[0] contains the scaling factor
```

The function has the following parameters:

-   **N**: order of the matrix A.
-   **A**: [`Complex128Array`][@stdlib/array/complex128] containing the LU-factored N-by-N matrix from zgetc2.
-   **strideA1**: stride of the first dimension of `A` (in complex elements).
-   **strideA2**: stride of the second dimension of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **RHS**: [`Complex128Array`][@stdlib/array/complex128] containing the right-hand side vector (overwritten with the solution).
-   **strideRHS**: stride for `RHS` (in complex elements).
-   **offsetRHS**: starting index for `RHS` (in complex elements).
-   **IPIV**: [`Int32Array`][mdn-int32array] containing row pivot indices from zgetc2 (0-based).
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **JPIV**: [`Int32Array`][mdn-int32array] containing column pivot indices from zgetc2 (0-based).
-   **strideJPIV**: stride for `JPIV`.
-   **offsetJPIV**: starting index for `JPIV`.
-   **scale**: [`Float64Array`][mdn-float64array] output where `scale[0]` receives the scaling factor.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine solves `A * X = scale * RHS` where A has been LU-factored with complete pivoting by [`zgetc2`][@stdlib/lapack/base/zgetc2]. The scaling factor is chosen to prevent overflow.
-   `IPIV` and `JPIV` are expected to be 0-based in the ndarray interface.
-   `zgesc2` is a complex (double-precision) version of [`dgesc2`][@stdlib/lapack/base/dgesc2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgesc2 = require( '@stdlib/lapack/base/zgesc2' );

var A = new Complex128Array( [ 4.0, 1.0, 0.353, 0.412, 2.0, -1.0, 1.882, 0.029 ] );
var RHS = new Complex128Array( [ 10.0, 3.0, 7.0, 4.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var JPIV = new Int32Array( [ 0, 1 ] );
var scale = new Float64Array( 1 );

zgesc2.ndarray( 2, A, 1, 2, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale );

var view = reinterpret( RHS, 0 );
console.log( 'Solution:', Array.from( view ) );
console.log( 'Scale:', scale[ 0 ] );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128
[@stdlib/lapack/base/zgetc2]: https://github.com/stdlib-js/lapack-base-zgetc2
[@stdlib/lapack/base/dgesc2]: https://github.com/stdlib-js/lapack-base-dgesc2
[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
