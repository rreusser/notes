<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# dlasd7

> @license Apache-2.0.

<section class="usage">

## Usage

```javascript
var dlasd7 = require( '@stdlib/lapack/base/dlasd7' );
```

#### dlasd7( icompq, nl, nr, sqre, d, z, ZW, VF, VFW, VL, VLW, alpha, beta, DSIGMA, IDX, IDXP, IDXQ, PERM, GIVCOL, LDGCOL, GIVNUM, LDGNUM )

@license Apache-2.0.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **icompq**: `icompq`.
-   **nl**: `nl`.
-   **nr**: `nr`.
-   **sqre**: `sqre`.
-   **d**: `d`.
-   **z**: `z`.
-   **ZW**: input array `ZW`.
-   **VF**: input array `VF`.
-   **VFW**: input array `VFW`.
-   **VL**: input array `VL`.
-   **VLW**: input array `VLW`.
-   **alpha**: scalar constant.
-   **beta**: scalar constant.
-   **DSIGMA**: input array `DSIGMA`.
-   **IDX**: input array `IDX`.
-   **IDXP**: input array `IDXP`.
-   **IDXQ**: input array `IDXQ`.
-   **PERM**: input array `PERM`.
-   **GIVCOL**: input array `GIVCOL`.
-   **LDGCOL**: leading dimension of `GCOL`.
-   **GIVNUM**: input array `GIVNUM`.
-   **LDGNUM**: leading dimension of `GNUM`.

#### dlasd7.ndarray( icompq, nl, nr, sqre, d, strideD, offsetD, z, strideZ, offsetZ, ZW, strideZW, offsetZW, VF, strideVF, offsetVF, VFW, strideVFW, offsetVFW, VL, strideVL, offsetVL, VLW, strideVLW, offsetVLW, alpha, beta, DSIGMA, strideDSIGMA, offsetDSIGMA, IDX, strideIDX, offsetIDX, IDXP, strideIDXP, offsetIDXP, IDXQ, strideIDXQ, offsetIDXQ, PERM, stridePERM, offsetPERM, GIVCOL, strideGIVCOL1, strideGIVCOL2, offsetGIVCOL, GIVNUM, strideGIVNUM1, strideGIVNUM2, offsetGIVNUM )

@license Apache-2.0, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideD**: stride length for `D`.
-   **offsetD**: starting index for `D`.
-   **strideZ**: stride length for `Z`.
-   **offsetZ**: starting index for `Z`.
-   **strideZW**: stride length for `ZW`.
-   **offsetZW**: starting index for `ZW`.
-   **strideVF**: stride length for `VF`.
-   **offsetVF**: starting index for `VF`.
-   **strideVFW**: stride length for `VFW`.
-   **offsetVFW**: starting index for `VFW`.
-   **strideVL**: stride length for `VL`.
-   **offsetVL**: starting index for `VL`.
-   **strideVLW**: stride length for `VLW`.
-   **offsetVLW**: starting index for `VLW`.
-   **strideDSIGMA**: stride length for `DSIGMA`.
-   **offsetDSIGMA**: starting index for `DSIGMA`.
-   **strideIDX**: stride length for `IDX`.
-   **offsetIDX**: starting index for `IDX`.
-   **strideIDXP**: stride length for `IDXP`.
-   **offsetIDXP**: starting index for `IDXP`.
-   **strideIDXQ**: stride length for `IDXQ`.
-   **offsetIDXQ**: starting index for `IDXQ`.
-   **stridePERM**: stride length for `PERM`.
-   **offsetPERM**: starting index for `PERM`.
-   **strideGIVCOL1**: stride of dimension 1 of `GIVCOL`.
-   **strideGIVCOL2**: stride of dimension 2 of `GIVCOL`.
-   **offsetGIVCOL**: starting index for `GIVCOL`.
-   **strideGIVNUM1**: stride of dimension 1 of `GIVNUM`.
-   **strideGIVNUM2**: stride of dimension 2 of `GIVNUM`.
-   **offsetGIVNUM**: starting index for `GIVNUM`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlasd7()` corresponds to the [LAPACK][lapack] level routine [`dlasd7`][lapack-dlasd7].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlasd7 = require( '@stdlib/lapack/base/dlasd7' );

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

[lapack]: https://www.netlib.org/lapack/explore-html/

[lapack-dlasd7]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlasd7.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->