# dlaed2

> Merge eigenvalues and deflate secular equation in divide and conquer

<section class="usage">

## Usage

```javascript
var dlaed2 = require( '@stdlib/lapack/base/dlaed2' );
```

#### dlaed2.ndarray( K, N, n1, d, strideD, offsetD, Q, strideQ1, strideQ2, offsetQ, INDXQ, strideINDXQ, offsetINDXQ, rho, z, strideZ, offsetZ, DLAMBDA, strideDLAMBDA, offsetDLAMBDA, w, strideW, offsetW, Q2, strideQ2, offsetQ2, INDX, strideINDX, offsetINDX, INDXC, strideINDXC, offsetINDXC, INDXP, strideINDXP, offsetINDXP, COLTYP, strideCOLTYP, offsetCOLTYP )

Merge eigenvalues and deflate secular equation in divide and conquer

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **K**: number of superdiagonals.
-   **N**: number of columns.
-   **n1**: n1.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **Q**: input matrix.
-   **strideQ1**: stride of the first dimension of `Q`.
-   **strideQ2**: stride of the second dimension of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **INDXQ**: input array.
-   **strideINDXQ**: stride length for `INDXQ`.
-   **offsetINDXQ**: starting index for `INDXQ`.
-   **rho**: rho.
-   **z**: input array.
-   **strideZ**: stride length for `z`.
-   **offsetZ**: starting index for `z`.
-   **DLAMBDA**: input array.
-   **strideDLAMBDA**: stride length for `DLAMBDA`.
-   **offsetDLAMBDA**: starting index for `DLAMBDA`.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `w`.
-   **Q2**: input array.
-   **strideQ2**: stride length for `Q2`.
-   **offsetQ2**: starting index for `Q2`.
-   **INDX**: input array.
-   **strideINDX**: stride length for `INDX`.
-   **offsetINDX**: starting index for `INDX`.
-   **INDXC**: input array.
-   **strideINDXC**: stride length for `INDXC`.
-   **offsetINDXC**: starting index for `INDXC`.
-   **INDXP**: input array.
-   **strideINDXP**: stride length for `INDXP`.
-   **offsetINDXP**: starting index for `INDXP`.
-   **COLTYP**: output array.
-   **strideCOLTYP**: stride length for `COLTYP`.
-   **offsetCOLTYP**: starting index for `COLTYP`.

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
