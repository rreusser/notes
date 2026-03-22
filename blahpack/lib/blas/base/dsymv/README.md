# dsymv

> Perform symmetric matrix-vector multiplication

<section class="usage">

## Usage

```javascript
var dsymv = require( '@stdlib/blas/base/dsymv' );
```

#### dsymv.ndarray( uplo, N, alpha, A, strideA1, strideA2, offsetA, x, strideX, offsetX, beta, y, strideY, offsetY )

Perform symmetric matrix-vector multiplication

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **alpha**: scalar constant.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
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
