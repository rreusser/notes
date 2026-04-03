# ztgsja

> Computes the generalized singular value decomposition of two complex upper triangular matrices.

<section class="usage">

## Usage

```javascript
var ztgsja = require( '@stdlib/lapack/base/ztgsja' );
```

#### ztgsja.ndarray( jobu, jobv, jobq, M, p, N, K, l, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, tola, tolb, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, U, strideU1, strideU2, offsetU, V, strideV1, strideV2, offsetV, Q, strideQ1, strideQ2, offsetQ, WORK, strideWORK, offsetWORK, ncycle )

Computes the generalized singular value decomposition of two complex upper triangular matrices.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var ztgsja = require( '@stdlib/lapack/base/ztgsja' );

var A = new Complex128Array( 4 );
var B = new Complex128Array( 4 );
var U = new Complex128Array( 4 );
var V = new Complex128Array( 4 );
var Q = new Complex128Array( 4 );
var WORK = new Complex128Array( 4 );
var ALPHA = new Float64Array( 2 );
var BETA = new Float64Array( 2 );
var ncycle = new Int32Array( 1 );

var info = ztgsja.ndarray( 'initialize', 'initialize', 'initialize', 2, 2, 2, 0, 2, A, 1, 2, 0, B, 1, 2, 0, 1e-14, 1e-14, ALPHA, 1, 0, BETA, 1, 0, U, 1, 2, 0, V, 1, 2, 0, Q, 1, 2, 0, WORK, 1, 0, ncycle );
// info => 0
```

The function has the following parameters:

-   **jobu**: specifies the operation type.
-   **jobv**: specifies the operation type.
-   **jobq**: specifies the operation type.
-   **M**: number of rows.
-   **p**: p.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **l**: l.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **tola**: tola.
-   **tolb**: tolb.
-   **ALPHA**: input array.
-   **strideALPHA**: stride length for `ALPHA`.
-   **offsetALPHA**: starting index for `ALPHA`.
-   **BETA**: input array.
-   **strideBETA**: stride length for `BETA`.
-   **offsetBETA**: starting index for `BETA`.
-   **U**: input matrix.
-   **strideU1**: stride of the first dimension of `U`.
-   **strideU2**: stride of the second dimension of `U`.
-   **offsetU**: starting index for `U`.
-   **V**: input matrix.
-   **strideV1**: stride of the first dimension of `V`.
-   **strideV2**: stride of the second dimension of `V`.
-   **offsetV**: starting index for `V`.
-   **Q**: input matrix.
-   **strideQ1**: stride of the first dimension of `Q`.
-   **strideQ2**: stride of the second dimension of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **ncycle**: ncycle.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The routine assumes that matrices `A` and `B` have the forms obtained by the preprocessing subroutine `ZGGSVP`.
-   The `jobu`, `jobv`, and `jobq` parameters accept `'initialize'`, `'compute-vectors'`, or `'none'`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var ztgsja = require( '@stdlib/lapack/base/ztgsja' );

var A = new Complex128Array( 4 );
var B = new Complex128Array( 4 );
var U = new Complex128Array( 4 );
var V = new Complex128Array( 4 );
var Q = new Complex128Array( 4 );
var WORK = new Complex128Array( 4 );
var ALPHA = new Float64Array( 2 );
var BETA = new Float64Array( 2 );
var ncycle = new Int32Array( 1 );

var info = ztgsja.ndarray( 'initialize', 'initialize', 'initialize', 2, 2, 2, 0, 2, A, 1, 2, 0, B, 1, 2, 0, 1e-14, 1e-14, ALPHA, 1, 0, BETA, 1, 0, U, 1, 2, 0, V, 1, 2, 0, Q, 1, 2, 0, WORK, 1, 0, ncycle );
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
