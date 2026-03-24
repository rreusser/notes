# dlaqps

> Computes a step of QR factorization with column pivoting using Level 3 BLAS

<section class="usage">

## Usage

```javascript
var dlaqps = require( '@stdlib/lapack/base/dlaqps' );
```

#### dlaqps.ndarray( M, N, offset, nb, kb, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, AUXV, strideAUXV, offsetAUXV, F, strideF1, strideF2, offsetF )

Computes a step of QR factorization with column pivoting using Level 3 BLAS

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **offset**: offset.
-   **nb**: nb.
-   **kb**: kb.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **JPVT**: input array.
-   **strideJPVT**: stride length for `JPVT`.
-   **offsetJPVT**: starting index for `JPVT`.
-   **TAU**: input array.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **VN1**: input array.
-   **strideVN1**: stride length for `VN1`.
-   **offsetVN1**: starting index for `VN1`.
-   **VN2**: input array.
-   **strideVN2**: stride length for `VN2`.
-   **offsetVN2**: starting index for `VN2`.
-   **AUXV**: input array.
-   **strideAUXV**: stride length for `AUXV`.
-   **offsetAUXV**: starting index for `AUXV`.
-   **F**: output matrix.
-   **strideF1**: stride of the first dimension of `F`.
-   **strideF2**: stride of the second dimension of `F`.
-   **offsetF**: starting index for `F`.

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
