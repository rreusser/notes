# zsyr2k

> Perform one of the symmetric rank-2k operations C := alpha*A*B**T + alpha*B*A**T + beta*C or C := alpha*A**T*B + alpha*B**T*A + beta*C.

<section class="usage">

## Usage

```javascript
var zsyr2k = require( '@stdlib/blas/base/zsyr2k' );
```

#### zsyr2k.ndarray( uplo, trans, N, K, alpha, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, beta, C, strideC1, strideC2, offsetC )

Perform one of the symmetric rank-2k operations C := alpha*A*B**T + alpha*B*A**T + beta*C or C := alpha*A**T*B + alpha*B**T*A + beta*C.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **N**: number of columns.
-   **K**: number of superdiagonals.
-   **alpha**: scalar constant.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: input matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **beta**: scalar constant.
-   **C**: output matrix.
-   **strideC1**: stride of the first dimension of `C`.
-   **strideC2**: stride of the second dimension of `C`.
-   **offsetC**: starting index for `C`.

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
