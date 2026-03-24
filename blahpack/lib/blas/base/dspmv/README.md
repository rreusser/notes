# dspmv

> Perform matrix-vector operation with a symmetric packed matrix

<section class="usage">

## Usage

```javascript
var dspmv = require( '@stdlib/blas/base/dspmv' );
```

#### dspmv.ndarray( uplo, N, alpha, AP, strideAP, offsetAP, x, strideX, offsetX, beta, y, strideY, offsetY )

Perform matrix-vector operation with a symmetric packed matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **alpha**: scalar constant.
-   **AP**: input array.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **x**: input array.
-   **strideX**: stride length for `x`.
-   **offsetX**: starting index for `x`.
-   **beta**: scalar constant.
-   **y**: output array.
-   **strideY**: stride length for `y`.
-   **offsetY**: starting index for `y`.

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
