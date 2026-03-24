# dtgsy2

> Solves the generalized Sylvester equation (unblocked)

<section class="usage">

## Usage

```javascript
var dtgsy2 = require( '@stdlib/lapack/base/dtgsy2' );
```

#### dtgsy2.ndarray( trans, ijob, M, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, C, strideC1, strideC2, offsetC, D, strideD1, strideD2, offsetD, E, strideE1, strideE2, offsetE, F, strideF1, strideF2, offsetF, scale, rdsum, rdscal, IWORK, strideIWORK, offsetIWORK, pq )

Solves the generalized Sylvester equation (unblocked)

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **trans**: specifies the operation type.
-   **ijob**: ijob.
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **C**: input matrix.
-   **strideC1**: stride of the first dimension of `C`.
-   **strideC2**: stride of the second dimension of `C`.
-   **offsetC**: starting index for `C`.
-   **D**: input matrix.
-   **strideD1**: stride of the first dimension of `D`.
-   **strideD2**: stride of the second dimension of `D`.
-   **offsetD**: starting index for `D`.
-   **E**: input matrix.
-   **strideE1**: stride of the first dimension of `E`.
-   **strideE2**: stride of the second dimension of `E`.
-   **offsetE**: starting index for `E`.
-   **F**: input matrix.
-   **strideF1**: stride of the first dimension of `F`.
-   **strideF2**: stride of the second dimension of `F`.
-   **offsetF**: starting index for `F`.
-   **scale**: scale.
-   **rdsum**: rdsum.
-   **rdscal**: rdscal.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **pq**: pq.

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
