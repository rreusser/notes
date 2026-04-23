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

# ztgevc

> ABS1: |re| + |im| (cheap complex absolute value).

<section class="usage">

## Usage

```javascript
var ztgevc = require( '@stdlib/lapack/base/ztgevc' );
```

#### ztgevc( order, side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, LDS, P, LDP, VL, LDVL, VR, LDVR, mm, M, WORK, strideWORK, RWORK, strideRWORK )

ABS1: |re| + |im| (cheap complex absolute value).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: specifies the side of the operation.
-   **howmny**: `howmny`.
-   **SELECT**: input array `SELECT`.
-   **strideSELECT**: stride length for `SELECT`.
-   **offsetSELECT**: starting index for `SELECT`.
-   **N**: number of columns.
-   **S**: input array `S`.
-   **LDS**: leading dimension of `S`.
-   **P**: input array `P`.
-   **LDP**: leading dimension of `P`.
-   **VL**: input array `VL`.
-   **LDVL**: leading dimension of `VL`.
-   **VR**: input array `VR`.
-   **LDVR**: leading dimension of `VR`.
-   **mm**: `mm`.
-   **M**: number of rows.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **RWORK**: input array `RWORK`.
-   **strideRWORK**: stride length for `RWORK`.

#### ztgevc.ndarray( side, howmny, SELECT, strideSELECT, offsetSELECT, N, S, strideS1, strideS2, offsetS, P, strideP1, strideP2, offsetP, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, mm, M, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

ABS1: |re| + |im| (cheap complex absolute value), using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideS1**: stride of dimension 1 of `S`.
-   **strideS2**: stride of dimension 2 of `S`.
-   **offsetS**: starting index for `S`.
-   **strideP1**: stride of dimension 1 of `P`.
-   **strideP2**: stride of dimension 2 of `P`.
-   **offsetP**: starting index for `P`.
-   **strideVL1**: stride of dimension 1 of `VL`.
-   **strideVL2**: stride of dimension 2 of `VL`.
-   **offsetVL**: starting index for `VL`.
-   **strideVR1**: stride of dimension 1 of `VR`.
-   **strideVR2**: stride of dimension 2 of `VR`.
-   **offsetVR**: starting index for `VR`.
-   **offsetWORK**: starting index for `WORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `ztgevc()` corresponds to the [LAPACK][lapack] level routine [`ztgevc`][lapack-ztgevc].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var ztgevc = require( '@stdlib/lapack/base/ztgevc' );

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

[lapack-ztgevc]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__ztgevc.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->