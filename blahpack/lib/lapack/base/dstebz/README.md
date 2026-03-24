# dstebz

> Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection

<section class="usage">

## Usage

```javascript
var dstebz = require( '@stdlib/lapack/base/dstebz' );
```

#### dstebz.ndarray( range, order, N, vl, vu, il, iu, abstol, d, strideD, offsetD, e, strideE, offsetE, M, nsplit, w, strideW, offsetW, IBLOCK, strideIBLOCK, offsetIBLOCK, ISPLIT, strideISPLIT, offsetISPLIT, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Computes selected eigenvalues of a real symmetric tridiagonal matrix by bisection

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **range**: specifies the operation type.
-   **order**: specifies the operation type.
-   **N**: number of columns.
-   **vl**: vl.
-   **vu**: vu.
-   **il**: il.
-   **iu**: iu.
-   **abstol**: abstol.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **M**: number of rows.
-   **nsplit**: nsplit.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **IBLOCK**: input array.
-   **strideIBLOCK**: stride length for `IBLOCK`.
-   **offsetIBLOCK**: starting index for `IBLOCK`.
-   **ISPLIT**: input array.
-   **strideISPLIT**: stride length for `ISPLIT`.
-   **offsetISPLIT**: starting index for `ISPLIT`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

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
