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

# zgeevx

> Computes eigenvalues, eigenvectors, and reciprocal condition numbers for a complex nonsymmetric matrix

<section class="usage">

## Usage

```javascript
var zgeevx = require( '@stdlib/lapack/base/zgeevx' );
```

#### zgeevx( order, balanc, jobvl, jobvr, sense, N, A, LDA, w, strideW, VL, LDVL, VR, LDVR, ilo, ihi, SCALE, strideSCALE, abnrm, RCONDE, strideRCONDE, RCONDV, strideRCONDV, WORK, strideWORK, lwork, RWORK, strideRWORK )

Computes eigenvalues, eigenvectors, and reciprocal condition numbers for a complex nonsymmetric matrix

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var out = zgeevx( /* ... */ );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **balanc**: specifies the operation type.
-   **jobvl**: specifies the operation type.
-   **jobvr**: specifies the operation type.
-   **sense**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **VL**: input matrix.
-   **LDVL**: leading dimension of `VL`.
-   **VR**: input matrix.
-   **LDVR**: leading dimension of `VR`.
-   **ilo**: ilo.
-   **ihi**: ihi.
-   **SCALE**: input array.
-   **strideSCALE**: stride length for `SCALE`.
-   **abnrm**: abnrm.
-   **RCONDE**: input array.
-   **strideRCONDE**: stride length for `RCONDE`.
-   **RCONDV**: input array.
-   **strideRCONDV**: stride length for `RCONDV`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **lwork**: lwork.
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.

#### zgeevx.ndarray( balanc, jobvl, jobvr, sense, N, A, strideA1, strideA2, offsetA, w, strideW, offsetW, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, ilo, ihi, SCALE, strideSCALE, offsetSCALE, abnrm, RCONDE, strideRCONDE, offsetRCONDE, RCONDV, strideRCONDV, offsetRCONDV, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK )

Computes eigenvalues, eigenvectors, and reciprocal condition numbers for a complex nonsymmetric matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var out = zgeevx( /* ... */ );
```

The function has the following additional parameters:

-   **balanc**: specifies the operation type.
-   **jobvl**: specifies the operation type.
-   **jobvr**: specifies the operation type.
-   **sense**: specifies the operation type.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `W`.
-   **VL**: input matrix.
-   **strideVL1**: stride of dimension 1 of `VL`.
-   **strideVL2**: stride of dimension 2 of `VL`.
-   **offsetVL**: starting index for `VL`.
-   **VR**: input matrix.
-   **strideVR1**: stride of dimension 1 of `VR`.
-   **strideVR2**: stride of dimension 2 of `VR`.
-   **offsetVR**: starting index for `VR`.
-   **ilo**: ilo.
-   **ihi**: ihi.
-   **SCALE**: input array.
-   **strideSCALE**: stride length for `SCALE`.
-   **offsetSCALE**: starting index for `SCALE`.
-   **abnrm**: abnrm.
-   **RCONDE**: input array.
-   **strideRCONDE**: stride length for `RCONDE`.
-   **offsetRCONDE**: starting index for `RCONDE`.
-   **RCONDV**: input array.
-   **strideRCONDV**: stride length for `RCONDV`.
-   **offsetRCONDV**: starting index for `RCONDV`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: lwork.
-   **RWORK**: output array.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Reciprocal condition number computation (`sense !== 'none'`) is not yet implemented and will throw an `Error`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var zgeevx = require( '@stdlib/lapack/base/zgeevx' );
// See examples/index.js
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
