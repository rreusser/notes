# zhetri

> Computes the inverse of a complex Hermitian matrix using the factorization computed by zhetrf.

<section class="usage">

## Usage

```javascript
var zhetri = require( '@stdlib/lapack/base/zhetri' );
```

#### zhetri.ndarray( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, WORK, strideWORK, offsetWORK )

Computes the inverse of a complex Hermitian matrix using the factorization computed by zhetrf.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );

// 1x1 Hermitian matrix A = [4+0i], already factored (trivial pivot):
var A = new Complex128Array( [ 4.0, 0.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var WORK = new Complex128Array( 1 );

var info = zhetri.ndarray( 'upper', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
// info => 0
// A => < 0.25, 0.0 >
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   IPIV uses 0-based indexing. Negative values (via bitwise NOT) indicate 2x2 pivot blocks.
-   The input matrix `A` must contain the factorization from `zhetrf`.
-   On success (`info === 0`), `A` is overwritten with the inverse.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhetri = require( '@stdlib/lapack/base/zhetri' );

// 1x1 Hermitian matrix A = [9+0i], already factored:
var A = new Complex128Array( [ 9.0, 0.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var WORK = new Complex128Array( 1 );

var info = zhetri.ndarray( 'lower', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
// info => 0
// A => ~< 0.1111, 0.0 >
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
