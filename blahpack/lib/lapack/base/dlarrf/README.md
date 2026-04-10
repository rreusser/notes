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

# dlarrf

> Finds a new relatively robust representation for eigenvalues

<section class="usage">

## Usage

```javascript
var dlarrf = require( '@stdlib/lapack/base/dlarrf' );
```

#### dlarrf( N, d, strideD, l, strideL, LD, strideLD, clstrt, clend, w, strideW, WGAP, strideWGAP, WERR, strideWERR, spdiam, clgapl, clgapr, pivmin, sigma, DPLUS, strideDPLUS, LPLUS, strideLPLUS, WORK, strideWORK )

Finds a new relatively robust representation for eigenvalues

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **l**: input array.
-   **strideL**: stride length for `l`.
-   **LD**: leading dimension of ``.
-   **strideLD**: stride length for `LD`.
-   **clstrt**: clstrt.
-   **clend**: clend.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **WGAP**: input array.
-   **strideWGAP**: stride length for `WGAP`.
-   **WERR**: input array.
-   **strideWERR**: stride length for `WERR`.
-   **spdiam**: spdiam.
-   **clgapl**: clgapl.
-   **clgapr**: clgapr.
-   **pivmin**: pivmin.
-   **sigma**: sigma.
-   **DPLUS**: input array.
-   **strideDPLUS**: stride length for `DPLUS`.
-   **LPLUS**: input array.
-   **strideLPLUS**: stride length for `LPLUS`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.

#### dlarrf.ndarray( N, d, strideD, offsetD, l, strideL, offsetL, LD, strideLD, offsetLD, clstrt, clend, w, strideW, offsetW, WGAP, strideWGAP, offsetWGAP, WERR, strideWERR, offsetWERR, spdiam, clgapl, clgapr, pivmin, sigma, DPLUS, strideDPLUS, offsetDPLUS, LPLUS, strideLPLUS, offsetLPLUS, WORK, strideWORK, offsetWORK )

Finds a new relatively robust representation for eigenvalues, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **N**: number of columns.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.
-   **l**: input array.
-   **strideL**: stride length for `l`.
-   **offsetL**: starting index for `L`.
-   **LD**: input array.
-   **strideLD**: stride length for `LD`.
-   **offsetLD**: starting index for `LD`.
-   **clstrt**: clstrt.
-   **clend**: clend.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `W`.
-   **WGAP**: input array.
-   **strideWGAP**: stride length for `WGAP`.
-   **offsetWGAP**: starting index for `WGAP`.
-   **WERR**: input array.
-   **strideWERR**: stride length for `WERR`.
-   **offsetWERR**: starting index for `WERR`.
-   **spdiam**: spdiam.
-   **clgapl**: clgapl.
-   **clgapr**: clgapr.
-   **pivmin**: pivmin.
-   **sigma**: sigma.
-   **DPLUS**: input array.
-   **strideDPLUS**: stride length for `DPLUS`.
-   **offsetDPLUS**: starting index for `DPLUS`.
-   **LPLUS**: input array.
-   **strideLPLUS**: stride length for `LPLUS`.
-   **offsetLPLUS**: starting index for `LPLUS`.
-   **WORK**: output array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

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
