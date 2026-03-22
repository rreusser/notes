# ztgevc

> Compute eigenvectors of a pair of complex upper triangular matrices

<section class="usage">

## Usage

```javascript
var ztgevc = require( '@stdlib/lapack/base/ztgevc' );
```

#### ztgevc.ndarray( side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, strideS1, strideS2, offsetS, P, strideP1, strideP2, offsetP, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Compute eigenvectors of a pair of complex upper triangular matrices

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **side**: specifies the operation type.
-   **howmny**: specifies the operation type.
-   **SELECT**: input array.
-   **strideSELECT**: stride length for `SELECT`.
-   **offsetSELECT**: starting index for `SELECT`.
-   **N**: number of columns.
-   **S**: input matrix.
-   **strideS1**: stride of the first dimension of `S`.
-   **strideS2**: stride of the second dimension of `S`.
-   **offsetS**: starting index for `S`.
-   **P**: input matrix.
-   **strideP1**: stride of the first dimension of `P`.
-   **strideP2**: stride of the second dimension of `P`.
-   **offsetP**: starting index for `P`.
-   **VL**: input matrix.
-   **strideVL1**: stride of the first dimension of `VL`.
-   **strideVL2**: stride of the second dimension of `VL`.
-   **offsetVL**: starting index for `VL`.
-   **VR**: input matrix.
-   **strideVR1**: stride of the first dimension of `VR`.
-   **strideVR2**: stride of the second dimension of `VR`.
-   **offsetVR**: starting index for `VR`.
-   **mm**: mm.
-   **M**: number of rows.
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
