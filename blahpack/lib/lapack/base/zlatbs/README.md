# zlatbs

> Complex triangular banded solve with scaling

<section class="usage">

## Usage

```javascript
var zlatbs = require( '@stdlib/lapack/base/zlatbs' );
```

#### zlatbs.ndarray( uplo, trans, diag, normin, N, kd, AB, strideAB1, strideAB2, offsetAB, x, strideX, offsetX, scale, CNORM, strideCNORM, offsetCNORM )

Complex triangular banded solve with scaling

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies the operation type.
-   **trans**: specifies the operation type.
-   **diag**: specifies the operation type.
-   **normin**: specifies the operation type.
-   **N**: number of columns.
-   **kd**: kd.
-   **AB**: input matrix.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **x**: input array.
-   **strideX**: stride length for `x`.
-   **offsetX**: starting index for `x`.
-   **scale**: scale.
-   **CNORM**: output array.
-   **strideCNORM**: stride length for `CNORM`.
-   **offsetCNORM**: starting index for `CNORM`.

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
