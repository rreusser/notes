# dlarrj

> Refine eigenvalue approximations using bisection given initial intervals.

<section class="usage">

## Usage

```javascript
var dlarrj = require( '@stdlib/lapack/base/dlarrj' );
```

#### dlarrj.ndarray( N, d, strideD, offsetD, E2, strideE2, offsetE2, ifirst, ilast, rtol, offset, w, strideW, offsetW, WERR, strideWERR, offsetWERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, pivmin, spdiam )

Refine eigenvalue approximations using bisection given initial intervals.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **E2**: input array.
-   **strideE2**: stride length for `E2`.
-   **offsetE2**: starting index for `E2`.
-   **ifirst**: ifirst.
-   **ilast**: ilast.
-   **rtol**: rtol.
-   **offset**: offset.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **WERR**: input array.
-   **strideWERR**: stride length for `WERR`.
-   **offsetWERR**: starting index for `WERR`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.
-   **pivmin**: pivmin.
-   **spdiam**: spdiam.

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
