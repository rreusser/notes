# zhpevx

> Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.

<section class="usage">

## Usage

```javascript
var zhpevx = require( '@stdlib/lapack/base/zhpevx' );
```

#### zhpevx.ndarray( jobz, range, uplo, N, AP, strideAP, offsetAP, vl, vu, il, iu, abstol, M, w, strideW, offsetW, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK, IWORK, strideIWORK, offsetIWORK, IFAIL, strideIFAIL, offsetIFAIL )

Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian matrix in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

var N = 2;
var AP = new Complex128Array( [ 3, 0, 1, -1, 1, 0 ] );
var w = new Float64Array( N );
var Z = new Complex128Array( N * N );
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( 7 * N );
var IWORK = new Int32Array( 5 * N );
var IFAIL = new Int32Array( N );
var out = { M: 0 };

zhpevx.ndarray( 'compute-vectors', 'all', 'upper', N, AP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, N, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
// out.M => 2
// w => [ ... eigenvalues ... ]
```

The function has the following parameters:

-   **jobz**: specifies the operation type.
-   **range**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **vl**: vl.
-   **vu**: vu.
-   **il**: il.
-   **iu**: iu.
-   **abstol**: abstol.
-   **M**: number of rows.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Z**: input matrix.
-   **strideZ1**: stride of the first dimension of `Z`.
-   **strideZ2**: stride of the second dimension of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: input array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.
-   **IWORK**: input array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **IFAIL**: output array.
-   **strideIFAIL**: stride length for `IFAIL`.
-   **offsetIFAIL**: starting index for `IFAIL`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Eigenvalues are real for a Hermitian matrix and stored in `w` (Float64Array).
-   Eigenvectors are complex and stored column-wise in `Z` (Complex128Array).
-   `AP`, `WORK`, `TAU` use complex-element strides; `RWORK`, `w` use real strides.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zhpevx = require( '@stdlib/lapack/base/zhpevx' );

var N = 4;
var AP = new Complex128Array( [
    4, 0, 1, 1, -2, -1, 2, 0,
    2, 0, 0, 0, 1, -1,
    3, 0, -2, 1,
    -1, 0
] );
var w = new Float64Array( N );
var Z = new Complex128Array( N * N );
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( 7 * N );
var IWORK = new Int32Array( 5 * N );
var IFAIL = new Int32Array( N );
var out = { M: 0 };

zhpevx( 'column-major', 'compute-vectors', 'all', 'lower', N, AP, 0, 0, 0, 0, 0, out, w, Z, N, WORK, RWORK, IWORK, IFAIL );
console.log( 'M:', out.M );
console.log( 'eigenvalues:', w );
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
