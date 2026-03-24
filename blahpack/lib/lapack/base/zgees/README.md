# zgees

> Compute eigenvalues and Schur decomposition of a complex matrix

<section class="usage">

## Usage

```javascript
var zgees = require( '@stdlib/lapack/base/zgees' );
```

#### zgees.ndarray( jobvs, sort, select, N, A, strideA1, strideA2, offsetA, sdim, w, strideW, offsetW, VS, strideVS1, strideVS2, offsetVS, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK, BWORK, strideBWORK, offsetBWORK )

Compute eigenvalues and Schur decomposition of a complex matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **jobvs**: specifies the operation type.
-   **sort**: specifies the operation type.
-   **select**: select.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **sdim**: sdim.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **VS**: input matrix.
-   **strideVS1**: stride of the first dimension of `VS`.
-   **strideVS2**: stride of the second dimension of `VS`.
-   **offsetVS**: starting index for `VS`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.
-   **RWORK**: input array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.
-   **BWORK**: output array.
-   **strideBWORK**: stride length for `BWORK`.
-   **offsetBWORK**: starting index for `BWORK`.

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
