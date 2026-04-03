# ztgsyl

> Solves the generalized Sylvester equation using a level-3 blocked algorithm.

<section class="usage">

## Usage

```javascript
var ztgsyl = require( '@stdlib/lapack/base/ztgsyl' );
```

#### ztgsyl.ndarray( trans, ijob, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, C, strideC1, strideC2, offsetC, D, strideD1, strideD2, offsetD, E, strideE1, strideE2, offsetE, F, strideF1, strideF2, offsetF, scale, dif, WORK, strideWORK, offsetWORK, lwork, IWORK, strideIWORK, offsetIWORK )

Solves the generalized Sylvester equation using a level-3 blocked algorithm.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
var scale = new Float64Array( 1 );
var dif = new Float64Array( 1 );
var IWORK = new Int32Array( 10 );

var info = ztgsyl.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, dif, null, 1, 0, -1, IWORK, 1, 0 );
// info => 0, C and F are overwritten with the solution
```

The function has the following parameters:

-   **trans**: specifies the operation type (`'no-transpose'` or `'conjugate-transpose'`).
-   **ijob**: job selector (0-4). Controls whether to solve only (0) or also estimate DIF (1-4).
-   **M**: number of rows of C and F, and order of matrices A and D.
-   **N**: number of columns of C and F, and order of matrices B and E.
-   **A**: `Complex128Array` containing the M-by-M upper triangular matrix A.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: `Complex128Array` containing the N-by-N upper triangular matrix B.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **C**: `Complex128Array` containing the M-by-N right-hand side / solution matrix C.
-   **strideC1**: stride of the first dimension of `C`.
-   **strideC2**: stride of the second dimension of `C`.
-   **offsetC**: starting index for `C`.
-   **D**: `Complex128Array` containing the M-by-M upper triangular matrix D.
-   **strideD1**: stride of the first dimension of `D`.
-   **strideD2**: stride of the second dimension of `D`.
-   **offsetD**: starting index for `D`.
-   **E**: `Complex128Array` containing the N-by-N upper triangular matrix E.
-   **strideE1**: stride of the first dimension of `E`.
-   **strideE2**: stride of the second dimension of `E`.
-   **offsetE**: starting index for `E`.
-   **F**: `Complex128Array` containing the M-by-N right-hand side / solution matrix F.
-   **strideF1**: stride of the first dimension of `F`.
-   **strideF2**: stride of the second dimension of `F`.
-   **offsetF**: starting index for `F`.
-   **scale**: `Float64Array` of length 1 for the output scaling factor.
-   **dif**: `Float64Array` of length 1 for the DIF estimate (when ijob >= 1).
-   **WORK**: `Complex128Array` workspace (needed for ijob 1-2; can be null for ijob 0).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: workspace size (ignored; auto-allocated internally if needed).
-   **IWORK**: `Int32Array` workspace of length M+N+6.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The matrices (A,D) and (B,E) must be in generalized complex Schur form (upper triangular).
-   When `trans` is `'no-transpose'`, the routine solves A*R - L*B = scale*C, D*R - L*E = scale*F.
-   When `trans` is `'conjugate-transpose'`, the routine solves A^H*R + D^H*L = scale*C, -R*B^H - L*E^H = scale*F.
-   The block size is hardcoded to 32 (equivalent to ILAENV defaults).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztgsyl = require( '@stdlib/lapack/base/ztgsyl' );

var A = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 0.5, 0.2, 2.0, -0.3 ] );
var B = new Complex128Array( [ 3.0, 0.1, 0.0, 0.0, 0.3, -0.1, 4.0, 0.2 ] );
var C = new Complex128Array( [ 1.0, 0.5, 3.0, 1.0, 2.0, -0.5, 4.0, 0.3 ] );
var D = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.5, -0.1 ] );
var E = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.1, 0.05, 2.0, 0.1 ] );
var F = new Complex128Array( [ 5.0, 1.0, 7.0, 0.5, 6.0, -1.0, 8.0, 0.2 ] );
var scale = new Float64Array( 1 );
var dif = new Float64Array( 1 );
var IWORK = new Int32Array( 10 );

var info = ztgsyl.ndarray( 'no-transpose', 0, 2, 2, A, 1, 2, 0, B, 1, 2, 0, C, 1, 2, 0, D, 1, 2, 0, E, 1, 2, 0, F, 1, 2, 0, scale, dif, null, 1, 0, -1, IWORK, 1, 0 );

var Cv = reinterpret( C, 0 );
console.log( 'info:', info );
console.log( 'scale:', scale[ 0 ] );
console.log( 'C (re/im):', Array.prototype.slice.call( Cv ) );
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
