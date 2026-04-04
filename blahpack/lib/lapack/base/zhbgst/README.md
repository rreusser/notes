# zhbgst

> Reduces a complex Hermitian-definite banded generalized eigenproblem to standard form.

<section class="usage">

## Usage

```javascript
var zhbgst = require( '@stdlib/lapack/base/zhbgst' );
```

#### zhbgst.ndarray( vect, uplo, N, ka, kb, AB, strideAB1, strideAB2, offsetAB, BB, strideBB1, strideBB2, offsetBB, X, strideX1, strideX2, offsetX, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Reduces a complex Hermitian-definite banded generalized eigenproblem to standard form.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Complex128Array = require( '@stdlib/array/complex128' );
var zhbgst = require( '@stdlib/lapack/base/zhbgst' );

var AB = new Complex128Array( 6 ); // 2x3 upper band
var BB = new Complex128Array( 3 ); // 1x3 diagonal
var X = new Complex128Array( 1 );
var WORK = new Complex128Array( 3 );
var RWORK = new Float64Array( 3 );

var info = zhbgst.ndarray( 'none', 'upper', 3, 1, 0, AB, 1, 2, 0, BB, 1, 1, 0, X, 1, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **vect**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **ka**: ka.
-   **kb**: kb.
-   **AB**: input matrix.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **BB**: input matrix.
-   **strideBB1**: stride of the first dimension of `BB`.
-   **strideBB2**: stride of the second dimension of `BB`.
-   **offsetBB**: starting index for `BB`.
-   **X**: input matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   B must have been previously factorized as S^H\*S by `zpbstf` (split Cholesky factorization).
-   The `vect` parameter controls whether the transformation matrix X is formed (`'update'`) or not (`'none'`).
-   All strides and offsets for complex arrays are in complex elements (not float64 elements).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhbgst = require( '@stdlib/lapack/base/zhbgst' );

var N = 3;
var ka = 1;
var kb = 0;
var AB = new Complex128Array( 2 * N );
var av = reinterpret( AB, 0 );
av[ 2 ] = 5.0; av[ 6 ] = 6.0; av[ 10 ] = 7.0;
av[ 4 ] = 1.0; av[ 5 ] = 0.5;
av[ 8 ] = 0.5; av[ 9 ] = -1.0;

var BB = new Complex128Array( N );
var bv = reinterpret( BB, 0 );
bv[ 0 ] = 2.0; bv[ 2 ] = 3.0; bv[ 4 ] = 2.0;

var X = new Complex128Array( 1 );
var WORK = new Complex128Array( N );
var RWORK = new Float64Array( N );

var info = zhbgst.ndarray( 'none', 'upper', N, ka, kb, AB, 1, 2, 0, BB, 1, 1, 0, X, 1, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
// => info: 0
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
