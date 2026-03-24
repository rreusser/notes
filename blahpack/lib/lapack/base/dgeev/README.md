# dgeev

> Computes eigenvalues and eigenvectors of a real general matrix

<section class="usage">

## Usage

```javascript
var dgeev = require( '@stdlib/lapack/base/dgeev' );
```

#### dgeev.ndarray( jobvl, jobvr, N, A, strideA1, strideA2, offsetA, WR, strideWR, offsetWR, WI, strideWI, offsetWI, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, WORK, strideWORK, offsetWORK, lwork )

Computes eigenvalues and eigenvectors of a real general matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **jobvl**: specifies the operation type.
-   **jobvr**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **WR**: input array.
-   **strideWR**: stride length for `WR`.
-   **offsetWR**: starting index for `WR`.
-   **WI**: input array.
-   **strideWI**: stride length for `WI`.
-   **offsetWI**: starting index for `WI`.
-   **VL**: input matrix.
-   **strideVL1**: stride of the first dimension of `VL`.
-   **strideVL2**: stride of the second dimension of `VL`.
-   **offsetVL**: starting index for `VL`.
-   **VR**: input matrix.
-   **strideVR1**: stride of the first dimension of `VR`.
-   **strideVR2**: stride of the second dimension of `VR`.
-   **offsetVR**: starting index for `VR`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.

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
