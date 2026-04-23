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

# dlaqr2

> Performs aggressive early deflation on an upper Hessenberg matrix.

<section class="usage">

## Usage

```javascript
var dlaqr2 = require( '@stdlib/lapack/base/dlaqr2' );
```

#### dlaqr2( wantt, wantz, N, ktop, kbot, nw, H, LDH, iloz, ihiz, Z, LDZ, SR, strideSR, SI, strideSI, V, LDV, nh, T, LDT, nv, WV, LDWV, WORK, strideWORK, lwork )

Performs aggressive early deflation on an upper Hessenberg matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **wantt**: `wantt`.
-   **wantz**: `wantz`.
-   **N**: number of columns.
-   **ktop**: `ktop`.
-   **kbot**: `kbot`.
-   **nw**: `nw`.
-   **H**: input array `H`.
-   **LDH**: leading dimension of `H`.
-   **iloz**: `iloz`.
-   **ihiz**: `ihiz`.
-   **Z**: input array `Z`.
-   **LDZ**: leading dimension of `Z`.
-   **SR**: input array `SR`.
-   **strideSR**: stride length for `SR`.
-   **SI**: input array `SI`.
-   **strideSI**: stride length for `SI`.
-   **V**: input array `V`.
-   **LDV**: leading dimension of `V`.
-   **nh**: `nh`.
-   **T**: input array `T`.
-   **LDT**: leading dimension of `T`.
-   **nv**: `nv`.
-   **WV**: input array `WV`.
-   **LDWV**: leading dimension of `WV`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **lwork**: `lwork`.

#### dlaqr2.ndarray( wantt, wantz, N, ktop, kbot, nw, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, ns, nd, SR, strideSR, offsetSR, SI, strideSI, offsetSI, V, strideV1, strideV2, offsetV, nh, T, strideT1, strideT2, offsetT, nv, WV, strideWV1, strideWV2, offsetWV, WORK, strideWORK, offsetWORK, lwork )

Performs aggressive early deflation on an upper Hessenberg matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideH1**: stride of dimension 1 of `H`.
-   **strideH2**: stride of dimension 2 of `H`.
-   **offsetH**: starting index for `H`.
-   **strideZ1**: stride of dimension 1 of `Z`.
-   **strideZ2**: stride of dimension 2 of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **ns**: `ns`.
-   **nd**: `nd`.
-   **offsetSR**: starting index for `SR`.
-   **offsetSI**: starting index for `SI`.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **strideWV1**: stride of dimension 1 of `WV`.
-   **strideWV2**: stride of dimension 2 of `WV`.
-   **offsetWV**: starting index for `WV`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlaqr2()` corresponds to the [LAPACK][lapack] level routine [`dlaqr2`][lapack-dlaqr2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlaqr2 = require( '@stdlib/lapack/base/dlaqr2' );

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

[lapack-dlaqr2]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlaqr2.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->