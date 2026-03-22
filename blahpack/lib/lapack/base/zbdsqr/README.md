# zbdsqr

> Compute SVD of a real bidiagonal matrix

<section class="usage">

## Usage

```javascript
var zbdsqr = require( '@stdlib/lapack/base/zbdsqr' );
```

#### zbdsqr.ndarray( uplo, N, ncvt, nru, ncc, d, strideD, offsetD, e, strideE, offsetE, VT, strideVT1, strideVT2, offsetVT, U, strideU1, strideU2, offsetU, C, strideC1, strideC2, offsetC, RWORK, strideRWORK, offsetRWORK )

Compute SVD of a real bidiagonal matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **ncvt**: ncvt.
-   **nru**: nru.
-   **ncc**: ncc.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **VT**: input matrix.
-   **strideVT1**: stride of the first dimension of `VT`.
-   **strideVT2**: stride of the second dimension of `VT`.
-   **offsetVT**: starting index for `VT`.
-   **U**: input matrix.
-   **strideU1**: stride of the first dimension of `U`.
-   **strideU2**: stride of the second dimension of `U`.
-   **offsetU**: starting index for `U`.
-   **C**: input matrix.
-   **strideC1**: stride of the first dimension of `C`.
-   **strideC2**: stride of the second dimension of `C`.
-   **offsetC**: starting index for `C`.
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
// TODO: Add examples
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
