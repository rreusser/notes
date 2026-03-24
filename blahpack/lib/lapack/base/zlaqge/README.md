# zlaqge

> Equilibrate a complex general matrix using row and column scalings

<section class="usage">

## Usage

```javascript
var zlaqge = require( '@stdlib/lapack/base/zlaqge' );
```

#### zlaqge.ndarray( M, N, A, strideA1, strideA2, offsetA, r, strideR, offsetR, c, strideC, offsetC, rowcnd, colcnd, amax, equed )

Equilibrate a complex general matrix using row and column scalings

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **r**: input array.
-   **strideR**: stride length for `r`.
-   **offsetR**: starting index for `r`.
-   **c**: output array.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **rowcnd**: rowcnd.
-   **colcnd**: colcnd.
-   **amax**: amax.
-   **equed**: specifies the operation type.

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
