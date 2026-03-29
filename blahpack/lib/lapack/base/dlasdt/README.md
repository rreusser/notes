# dlasdt

> Create a tree of subproblems for bidiagonal divide and conquer.

<section class="usage">

## Usage

```javascript
var dlasdt = require( '@stdlib/lapack/base/dlasdt' );
```

#### dlasdt.ndarray( N, lvl, nd, INODE, strideINODE, offsetINODE, NDIML, strideNDIML, offsetNDIML, NDIMR, strideNDIMR, offsetNDIMR, msub )

Create a tree of subproblems for bidiagonal divide and conquer.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **lvl**: lvl.
-   **nd**: nd.
-   **INODE**: input array.
-   **strideINODE**: stride length for `INODE`.
-   **offsetINODE**: starting index for `INODE`.
-   **NDIML**: input array.
-   **strideNDIML**: stride length for `NDIML`.
-   **offsetNDIML**: starting index for `NDIML`.
-   **NDIMR**: output array.
-   **strideNDIMR**: stride length for `NDIMR`.
-   **offsetNDIMR**: starting index for `NDIMR`.
-   **msub**: msub.

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
