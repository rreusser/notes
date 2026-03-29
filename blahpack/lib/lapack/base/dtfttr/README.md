# dtfttr

> Copy a triangular matrix from Rectangular Full Packed format to standard full format

<section class="usage">

## Usage

```javascript
var dtfttr = require( '@stdlib/lapack/base/dtfttr' );
```

#### dtfttr.ndarray( transr, uplo, N, ARF, strideARF, offsetARF, A, strideA1, strideA2, offsetA, lda )

Copy a triangular matrix from Rectangular Full Packed format to standard full format

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **transr**: specifies the operation type.
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **ARF**: input array.
-   **strideARF**: stride length for `ARF`.
-   **offsetARF**: starting index for `ARF`.
-   **A**: output matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **lda**: lda.

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
