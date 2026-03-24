# dlaebz

> Auxiliary bisection routine for tridiagonal eigenvalue computation

<section class="usage">

## Usage

```javascript
var dlaebz = require( '@stdlib/lapack/base/dlaebz' );
```

#### dlaebz.ndarray( ijob, nitmax, N, minp, nbmin, abstol, reltol, pivmin, d, strideD, offsetD, e, strideE, offsetE, E2, strideE2, offsetE2, NVAL, strideNVAL, offsetNVAL, AB, strideAB1, strideAB2, offsetAB, c, strideC, offsetC, mout, NAB, strideNAB1, strideNAB2, offsetNAB, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Auxiliary bisection routine for tridiagonal eigenvalue computation

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **ijob**: ijob.
-   **nitmax**: nitmax.
-   **N**: number of columns.
-   **minp**: minp.
-   **nbmin**: nbmin.
-   **abstol**: abstol.
-   **reltol**: reltol.
-   **pivmin**: pivmin.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **E2**: input array.
-   **strideE2**: stride length for `E2`.
-   **offsetE2**: starting index for `E2`.
-   **NVAL**: input array.
-   **strideNVAL**: stride length for `NVAL`.
-   **offsetNVAL**: starting index for `NVAL`.
-   **AB**: input matrix.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **c**: input array.
-   **strideC**: stride length for `c`.
-   **offsetC**: starting index for `c`.
-   **mout**: mout.
-   **NAB**: input matrix.
-   **strideNAB1**: stride of the first dimension of `NAB`.
-   **strideNAB2**: stride of the second dimension of `NAB`.
-   **offsetNAB**: starting index for `NAB`.
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
