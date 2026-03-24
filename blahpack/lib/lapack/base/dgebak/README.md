# dgebak

> Back-transforms eigenvectors after balancing by dgebal

<section class="usage">

## Usage

```javascript
var dgebak = require( '@stdlib/lapack/base/dgebak' );
```

#### dgebak.ndarray( job, side, N, ilo, ihi, SCALE, strideSCALE, offsetSCALE, M, V, strideV1, strideV2, offsetV )

Back-transforms eigenvectors after balancing by dgebal

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: specifies the operation type.
-   **side**: specifies the operation type.
-   **N**: number of columns.
-   **ilo**: ilo.
-   **ihi**: ihi.
-   **SCALE**: input array.
-   **strideSCALE**: stride length for `SCALE`.
-   **offsetSCALE**: starting index for `SCALE`.
-   **M**: number of rows.
-   **V**: output matrix.
-   **strideV1**: stride of the first dimension of `V`.
-   **strideV2**: stride of the second dimension of `V`.
-   **offsetV**: starting index for `V`.

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
