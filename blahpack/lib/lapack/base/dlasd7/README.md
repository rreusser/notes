# dlasd7

> Merge two sets of singular values together into a single sorted set and deflate.

<section class="usage">

## Usage

```javascript
var dlasd7 = require( '@stdlib/lapack/base/dlasd7' );
```

#### dlasd7.ndarray( icompq, nl, nr, sqre, K, d, strideD, offsetD, z, strideZ, offsetZ, ZW, strideZW, offsetZW, VF, strideVF, offsetVF, VFW, strideVFW, offsetVFW, VL, strideVL, offsetVL, VLW, strideVLW, offsetVLW, alpha, beta, DSIGMA, strideDSIGMA, offsetDSIGMA, IDX, strideIDX, offsetIDX, IDXP, strideIDXP, offsetIDXP, IDXQ, strideIDXQ, offsetIDXQ, PERM, stridePERM, offsetPERM, givptr, GIVCOL, strideGIVCOL1, strideGIVCOL2, offsetGIVCOL, GIVNUM, strideGIVNUM1, strideGIVNUM2, offsetGIVNUM, c, s )

Merge two sets of singular values together into a single sorted set and deflate.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **icompq**: icompq.
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
-   **ZW**: input array.
-   **strideZW**: stride length for `ZW`.
-   **offsetZW**: starting index for `ZW`.
-   **VF**: input array.
-   **strideVF**: stride length for `VF`.
-   **offsetVF**: starting index for `VF`.
-   **VFW**: input array.
-   **strideVFW**: stride length for `VFW`.
-   **offsetVFW**: starting index for `VFW`.
-   **VL**: input array.
-   **strideVL**: stride length for `VL`.
-   **offsetVL**: starting index for `VL`.
-   **VLW**: input array.
-   **strideVLW**: stride length for `VLW`.
-   **offsetVLW**: starting index for `VLW`.
-   **alpha**: scalar constant.
-   **beta**: scalar constant.
-   **DSIGMA**: input array.
-   **strideDSIGMA**: stride length for `DSIGMA`.
-   **offsetDSIGMA**: starting index for `DSIGMA`.
-   **IDX**: input array.
-   **strideIDX**: stride length for `IDX`.
-   **offsetIDX**: starting index for `IDX`.
-   **IDXP**: input array.
-   **strideIDXP**: stride length for `IDXP`.
-   **offsetIDXP**: starting index for `IDXP`.
-   **IDXQ**: input array.
-   **strideIDXQ**: stride length for `IDXQ`.
-   **offsetIDXQ**: starting index for `IDXQ`.
-   **PERM**: input array.
-   **stridePERM**: stride length for `PERM`.
-   **offsetPERM**: starting index for `PERM`.
-   **givptr**: givptr.
-   **GIVCOL**: input matrix.
-   **strideGIVCOL1**: stride of the first dimension of `GIVCOL`.
-   **strideGIVCOL2**: stride of the second dimension of `GIVCOL`.
-   **offsetGIVCOL**: starting index for `GIVCOL`.
-   **GIVNUM**: output matrix.
-   **strideGIVNUM1**: stride of the first dimension of `GIVNUM`.
-   **strideGIVNUM2**: stride of the second dimension of `GIVNUM`.
-   **offsetGIVNUM**: starting index for `GIVNUM`.
-   **c**: c.
-   **s**: s.

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
