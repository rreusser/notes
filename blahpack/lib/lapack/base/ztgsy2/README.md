# ztgsy2

> Solves the generalized Sylvester matrix equation for small subsystems.

<section class="usage">

## Usage

```javascript
var ztgsy2 = require( '@stdlib/lapack/base/ztgsy2' );
```

#### ztgsy2.ndarray( trans, ijob, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, C, strideC1, strideC2, offsetC, D, strideD1, strideD2, offsetD, E, strideE1, strideE2, offsetE, F, strideF1, strideF2, offsetF, scale, rdsum, rdscal )

Solves the generalized Sylvester matrix equation for small subsystems.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
var scale = new Float64Array( 1 );
var rdsum = new Float64Array( [ 0.0 ] );
var rdscal = new Float64Array( [ 1.0 ] );

var info = ztgsy2.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, rdsum, rdscal );
// C and F are overwritten with the solution
```

The function has the following parameters:

-   **trans**: `'no-transpose'` or `'conjugate-transpose'`.
-   **ijob**: `0` (solve only) or `2` (solve + estimate DIF via zlatdf).
-   **M**: number of rows in C, F and order of (A,D).
-   **N**: number of columns in C, F and order of (B,E).
-   **A**: `Complex128Array`, M-by-M upper triangular matrix.
-   **strideA1**: stride of the first dimension of `A` (in complex elements).
-   **strideA2**: stride of the second dimension of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **B**: `Complex128Array`, N-by-N upper triangular matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **C**: `Complex128Array`, M-by-N right-hand side / solution (overwritten).
-   **strideC1**: stride of the first dimension of `C`.
-   **strideC2**: stride of the second dimension of `C`.
-   **offsetC**: starting index for `C`.
-   **D**: `Complex128Array`, M-by-M upper triangular matrix.
-   **strideD1**: stride of the first dimension of `D`.
-   **strideD2**: stride of the second dimension of `D`.
-   **offsetD**: starting index for `D`.
-   **E**: `Complex128Array`, N-by-N upper triangular matrix.
-   **strideE1**: stride of the first dimension of `E`.
-   **strideE2**: stride of the second dimension of `E`.
-   **offsetE**: starting index for `E`.
-   **F**: `Complex128Array`, M-by-N right-hand side / solution (overwritten).
-   **strideF1**: stride of the first dimension of `F`.
-   **strideF2**: stride of the second dimension of `F`.
-   **offsetF**: starting index for `F`.
-   **scale**: `Float64Array` of length 1 receiving the scaling factor.
-   **rdsum**: `Float64Array` of length 1, in/out (used when ijob > 0).
-   **rdscal**: `Float64Array` of length 1, in/out (used when ijob > 0).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Complex Schur form is upper triangular, so ztgsy2 only handles 1x1 blocks (unlike dtgsy2 which handles quasi-triangular 2x2 blocks).
-   When TRANS = `'no-transpose'`, the routine solves: A\*R - L\*B = scale\*C, D\*R - L\*E = scale\*F.
-   When TRANS = `'conjugate-transpose'`, the routine solves: A^H\*R + D^H\*L = scale\*C, -R\*B^H - L\*E^H = scale\*F.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztgsy2 = require( '@stdlib/lapack/base/ztgsy2' );

var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
var scale = new Float64Array( 1 );
var rdsum = new Float64Array( [ 0.0 ] );
var rdscal = new Float64Array( [ 1.0 ] );

var info = ztgsy2.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, rdsum, rdscal );

var Cv = reinterpret( C, 0 );
console.log( 'info:', info );
console.log( 'scale:', scale[ 0 ] );
console.log( 'C:', Array.prototype.slice.call( Cv ) );
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
