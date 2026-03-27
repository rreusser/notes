# ztbmv

> Perform one of the matrix-vector operations x := A*x, x := A**T*x, or x := A**H*x, where A is a triangular band matrix

<section class="usage">

## Usage

```javascript
var ztbmv = require( '@stdlib/blas/base/ztbmv' );
```

#### ztbmv.ndarray( uplo, trans, diag, N, K, A, strideA1, strideA2, offsetA, x, strideX, offsetX )

Perform one of the matrix-vector operations x := A*x, x := A**T*x, or x := A**H*x, where A is a triangular band matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **diag**: specifies the operation type.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **x**: output array.
-   **strideX**: stride length for `x`.
-   **offsetX**: starting index for `x`.

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
