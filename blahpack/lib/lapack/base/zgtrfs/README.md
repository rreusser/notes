# zgtrfs

> Improve the computed solution to a complex tridiagonal system and provide error bounds.

<section class="usage">

## Usage

```javascript
var zgtrfs = require( '@stdlib/lapack/base/zgtrfs' );
```

#### zgtrfs.ndarray( trans, N, nrhs, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DLF, strideDLF, offsetDLF, DF, strideDF, offsetDF, DUF, strideDUF, offsetDUF, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Improves the computed solution to a complex system of linear equations `op(A) * X = B` where A is a complex tridiagonal matrix (using the LU factorization from `zgttrf`) and provides forward and backward error bounds.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgttrf = require( '@stdlib/lapack/base/zgttrf' );
var zgttrs = require( '@stdlib/lapack/base/zgttrs' );

var n = 4;
var DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
var DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
var DU2 = new Complex128Array( n );
var IPIV = new Int32Array( n );

zgttrf.ndarray( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );

var B = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
var X = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );

zgttrs.ndarray( 'no-transpose', n, 1, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n*2, 0 );

var WORK = new Complex128Array( 2 * n );
var RWORK = new Float64Array( n );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );

var info = zgtrfs.ndarray( 'no-transpose', n, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **trans**: specifies the form of the system: `no-transpose`, `transpose`, or `conjugate-transpose`.
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand side columns.
-   **DL**: sub-diagonal of original A (length N-1), `Complex128Array`.
-   **strideDL**: stride for `DL` (complex elements).
-   **offsetDL**: starting index for `DL` (complex elements).
-   **d**: diagonal of original A (length N), `Complex128Array`.
-   **strideD**: stride for `d` (complex elements).
-   **offsetD**: starting index for `d` (complex elements).
-   **DU**: super-diagonal of original A (length N-1), `Complex128Array`.
-   **strideDU**: stride for `DU` (complex elements).
-   **offsetDU**: starting index for `DU` (complex elements).
-   **DLF**: factored sub-diagonal from `zgttrf`, `Complex128Array`.
-   **strideDLF**: stride for `DLF` (complex elements).
-   **offsetDLF**: starting index for `DLF` (complex elements).
-   **DF**: factored diagonal from `zgttrf`, `Complex128Array`.
-   **strideDF**: stride for `DF` (complex elements).
-   **offsetDF**: starting index for `DF` (complex elements).
-   **DUF**: factored super-diagonal from `zgttrf`, `Complex128Array`.
-   **strideDUF**: stride for `DUF` (complex elements).
-   **offsetDUF**: starting index for `DUF` (complex elements).
-   **DU2**: second superdiagonal from `zgttrf`, `Complex128Array`.
-   **strideDU2**: stride for `DU2` (complex elements).
-   **offsetDU2**: starting index for `DU2` (complex elements).
-   **IPIV**: pivot indices from `zgttrf` (0-based), `Int32Array`.
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix, `Complex128Array`.
-   **strideB1**: stride of the first dimension of `B` (complex elements).
-   **strideB2**: stride of the second dimension of `B` (complex elements).
-   **offsetB**: starting index for `B` (complex elements).
-   **X**: solution matrix (refined on output), `Complex128Array`.
-   **strideX1**: stride of the first dimension of `X` (complex elements).
-   **strideX2**: stride of the second dimension of `X` (complex elements).
-   **offsetX**: starting index for `X` (complex elements).
-   **FERR**: output forward error bounds, `Float64Array`.
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds, `Float64Array`.
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace (length >= 2\*N), `Complex128Array`.
-   **strideWORK**: stride for `WORK` (complex elements).
-   **offsetWORK**: starting index for `WORK` (complex elements).
-   **RWORK**: real workspace (length >= N), `Float64Array`.
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine uses iterative refinement to improve the solution computed by `zgttrs` and provides componentwise forward and backward error bounds.
-   IPIV values must be 0-based (as produced by the JS `zgttrf`).
-   Complex arrays use complex-element strides and offsets.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( '@stdlib/lapack/base/zgttrf' );
var zgttrs = require( '@stdlib/lapack/base/zgttrs' );
var zgtrfs = require( '@stdlib/lapack/base/zgtrfs' );

var n = 4;
var DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );

var DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
var DU2 = new Complex128Array( n );
var IPIV = new Int32Array( n );

zgttrf.ndarray( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );

var B = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
var X = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
zgttrs.ndarray( 'no-transpose', n, 1, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n*2, 0 );

var WORK = new Complex128Array( 2 * n );
var RWORK = new Float64Array( n );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );

var info = zgtrfs.ndarray( 'no-transpose', n, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

console.log( 'info:', info );
console.log( 'X (Float64 view):', reinterpret( X, 0 ) );
console.log( 'FERR:', FERR );
console.log( 'BERR:', BERR );
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
