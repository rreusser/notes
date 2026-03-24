# dlagts

> Solves a tridiagonal system factored by dlagtf

<section class="usage">

## Usage

```javascript
var dlagts = require( '@stdlib/lapack/base/dlagts' );
```

#### dlagts.ndarray( job, N, a, strideA, offsetA, b, strideB, offsetB, c, strideC, offsetC, d, strideD, offsetD, IN, strideIN, offsetIN, y, strideY, offsetY, tol )

Solves a tridiagonal system factored by dlagtf

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: job.
-   **N**: number of columns.
-   **a**: input array.
-   **strideA**: stride length for `a`.
-   **offsetA**: starting index for `a`.
-   **b**: input array.
-   **strideB**: stride length for `b`.
-   **offsetB**: starting index for `b`.
-   **c**: input array.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **IN**: input array.
-   **strideIN**: stride length for `IN`.
-   **offsetIN**: starting index for `IN`.
-   **y**: output array.
-   **strideY**: stride length for `y`.
-   **offsetY**: starting index for `y`.
-   **tol**: tol.

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
