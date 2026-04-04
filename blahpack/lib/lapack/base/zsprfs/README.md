# zsprfs

> Improves the computed solution to a complex system A * X = B where A is symmetric in packed storage and provides error bounds.

<section class="usage">

## Usage

```javascript
var zsprfs = require( '@stdlib/lapack/base/zsprfs' );
```

#### zsprfs.ndarray( uplo, N, nrhs, AP, strideAP, offsetAP, AFP, strideAFP, offsetAFP, IPIV, strideIPIV, offsetIPIV, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Improves the computed solution to a complex system A * X = B where A is symmetric in packed storage and provides error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// See examples/index.js for usage
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **nrhs**: nrhs.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **AFP**: input array.
-   **strideAFP**: stride length for `AFP`.
-   **offsetAFP**: starting index for `AFP`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: input matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: input array.
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: input array.
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
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

-   `zsprfs()` iteratively refines the solution of `A*X = B` where `A` is complex symmetric (not Hermitian) and stored in packed format. WORK and RWORK are allocated internally.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
// See examples/index.js for usage
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
