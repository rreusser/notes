# zggqrf

> Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B.

<section class="usage">

## Usage

```javascript
var zggqrf = require( '@stdlib/lapack/base/zggqrf' );
```

#### zggqrf.ndarray( N, M, p, A, strideA1, strideA2, offsetA, TAUA, strideTAUA, offsetTAUA, B, strideB1, strideB2, offsetB, TAUB, strideTAUB, offsetTAUB, WORK, strideWORK, offsetWORK, lwork )

Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B:

```text
A = Q*R,        B = Q*T*Z,
```

where Q is an N-by-N unitary matrix, Z is a P-by-P unitary matrix, and R and T are upper trapezoidal/triangular.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 2, 1, 1, 0, 1, 2, 4, 1, 3, 0, 2, -1, 3, -1, 2, 0, 5, 2 ] );
var TAUA = new Complex128Array( 3 );
var B = new Complex128Array( [ 1, 0.5, 3, 0, 2, 1, 2, 1, 1, -1, 3, 0, 1, -1, 2, 0.5, 1, 1 ] );
var TAUB = new Complex128Array( 3 );
var WORK = new Complex128Array( 192 );

var info = zggqrf.ndarray( 3, 3, 3, A, 1, 3, 0, TAUA, 1, 0, B, 1, 3, 0, TAUB, 1, 0, WORK, 1, 0, 192 );
// info => 0
```

The function has the following parameters:

-   **N**: number of rows of A and B.
-   **M**: number of columns of A.
-   **p**: number of columns of B.
-   **A**: `Complex128Array` input N-by-M matrix (overwritten with R and reflectors).
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **TAUA**: `Complex128Array` output scalar factors of reflectors for Q.
-   **strideTAUA**: stride length for `TAUA`.
-   **offsetTAUA**: starting index for `TAUA`.
-   **B**: `Complex128Array` input N-by-P matrix (overwritten with T and reflectors).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **TAUB**: `Complex128Array` output scalar factors of reflectors for Z.
-   **strideTAUB**: stride length for `TAUB`.
-   **offsetTAUB**: starting index for `TAUB`.
-   **WORK**: `Complex128Array` workspace array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: length of workspace.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   All strides and offsets are in complex elements, not Float64 indices.
-   The workspace is allocated internally by the subroutines; the WORK/lwork parameters are kept for API consistency.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zggqrf = require( '@stdlib/lapack/base/zggqrf' );

var A = new Complex128Array( [ 5, 2 ] );
var TAUA = new Complex128Array( 1 );
var B = new Complex128Array( [ 3, -1 ] );
var TAUB = new Complex128Array( 1 );
var WORK = new Complex128Array( 64 );

var info = zggqrf.ndarray( 1, 1, 1, A, 1, 1, 0, TAUA, 1, 0, B, 1, 1, 0, TAUB, 1, 0, WORK, 1, 0, 64 );
console.log( 'info:', info );
console.log( 'A:', reinterpret( A, 0 ) );
console.log( 'B:', reinterpret( B, 0 ) );
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
