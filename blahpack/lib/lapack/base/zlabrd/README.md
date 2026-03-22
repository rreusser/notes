# zlabrd

> Reduce first NB rows/columns to bidiagonal form

<section class="usage">

## Usage

```javascript
var zlabrd = require( '@stdlib/lapack/base/zlabrd' );
```

#### zlabrd.ndarray( M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY )

Reduce first NB rows/columns to bidiagonal form

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **nb**: nb.
-   **A**: input matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `e`.
-   **TAUQ**: input array.
-   **strideTAUQ**: stride length for `TAUQ`.
-   **offsetTAUQ**: starting index for `TAUQ`.
-   **TAUP**: input array.
-   **strideTAUP**: stride length for `TAUP`.
-   **offsetTAUP**: starting index for `TAUP`.
-   **X**: input matrix.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **Y**: output matrix.
-   **strideY1**: stride of the first dimension of `Y`.
-   **strideY2**: stride of the second dimension of `Y`.
-   **offsetY**: starting index for `Y`.

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
