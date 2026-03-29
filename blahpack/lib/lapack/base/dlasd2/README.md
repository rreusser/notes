# dlasd2

> Merge two sets of singular values in bidiagonal SVD divide and conquer

<section class="usage">

## Usage

```javascript
var dlasd2 = require( '@stdlib/lapack/base/dlasd2' );
```

#### dlasd2.ndarray( nl, nr, sqre, K, d, strideD, offsetD, z, strideZ, offsetZ, alpha, beta, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, DSIGMA, strideDSIGMA, offsetDSIGMA, U2, strideU21, strideU22, offsetU2, VT2, strideVT21, strideVT22, offsetVT2, IDXP, strideIDXP, offsetIDXP, IDX, strideIDX, offsetIDX, IDXC, strideIDXC, offsetIDXC, IDXQ, strideIDXQ, offsetIDXQ, COLTYP, strideCOLTYP, offsetCOLTYP )

Merge two sets of singular values in bidiagonal SVD divide and conquer

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **nl**: nl.
-   **nr**: nr.
-   **sqre**: sqre.
-   **K**: number of superdiagonals.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `d`.
-   **z**: input array.
-   **strideZ**: stride length for `z`.
-   **offsetZ**: starting index for `z`.
-   **alpha**: scalar constant.
-   **beta**: scalar constant.
-   **U**: input matrix.
-   **strideU1**: stride of the first dimension of `U`.
-   **strideU2**: stride of the second dimension of `U`.
-   **offsetU**: starting index for `U`.
-   **VT**: input matrix.
-   **strideVT1**: stride of the first dimension of `VT`.
-   **strideVT2**: stride of the second dimension of `VT`.
-   **offsetVT**: starting index for `VT`.
-   **DSIGMA**: input array.
-   **strideDSIGMA**: stride length for `DSIGMA`.
-   **offsetDSIGMA**: starting index for `DSIGMA`.
-   **U2**: input matrix.
-   **strideU21**: stride of the first dimension of `U2`.
-   **strideU22**: stride of the second dimension of `U2`.
-   **offsetU2**: starting index for `U2`.
-   **VT2**: input matrix.
-   **strideVT21**: stride of the first dimension of `VT2`.
-   **strideVT22**: stride of the second dimension of `VT2`.
-   **offsetVT2**: starting index for `VT2`.
-   **IDXP**: input array.
-   **strideIDXP**: stride length for `IDXP`.
-   **offsetIDXP**: starting index for `IDXP`.
-   **IDX**: input array.
-   **strideIDX**: stride length for `IDX`.
-   **offsetIDX**: starting index for `IDX`.
-   **IDXC**: input array.
-   **strideIDXC**: stride length for `IDXC`.
-   **offsetIDXC**: starting index for `IDXC`.
-   **IDXQ**: input array.
-   **strideIDXQ**: stride length for `IDXQ`.
-   **offsetIDXQ**: starting index for `IDXQ`.
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
