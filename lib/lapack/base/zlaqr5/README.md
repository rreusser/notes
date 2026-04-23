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

# zlaqr5

> Complex multi-shift QR sweep.

<section class="usage">

## Usage

```javascript
var zlaqr5 = require( '@stdlib/lapack/base/zlaqr5' );
```

#### zlaqr5( v, idx )

Complex multi-shift QR sweep.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **v**: `v`.
-   **idx**: `idx`.

#### zlaqr5.ndarray( wantt, wantz, kacc22, N, ktop, kbot, nshfts, s, strideS, offsetS, H, strideH1, strideH2, offsetH, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, V, strideV1, strideV2, offsetV, U, strideU1, strideU2, offsetU, nv, WV, strideWV1, strideWV2, offsetWV, nh, WH, strideWH1, strideWH2, offsetWH )

Complex multi-shift QR sweep, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **wantt**: `wantt`.
-   **wantz**: `wantz`.
-   **kacc22**: `kacc22`.
-   **N**: number of columns.
-   **ktop**: `ktop`.
-   **kbot**: `kbot`.
-   **nshfts**: `nshfts`.
-   **s**: `s`.
-   **strideS**: stride length for `S`.
-   **offsetS**: starting index for `S`.
-   **H**: input array `H`.
-   **strideH1**: stride of dimension 1 of `H`.
-   **strideH2**: stride of dimension 2 of `H`.
-   **offsetH**: starting index for `H`.
-   **iloz**: `iloz`.
-   **ihiz**: `ihiz`.
-   **Z**: input array `Z`.
-   **strideZ1**: stride of dimension 1 of `Z`.
-   **strideZ2**: stride of dimension 2 of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **V**: input array `V`.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
-   **U**: input array `U`.
-   **strideU1**: stride of dimension 1 of `U`.
-   **strideU2**: stride of dimension 2 of `U`.
-   **offsetU**: starting index for `U`.
-   **nv**: `nv`.
-   **WV**: input array `WV`.
-   **strideWV1**: stride of dimension 1 of `WV`.
-   **strideWV2**: stride of dimension 2 of `WV`.
-   **offsetWV**: starting index for `WV`.
-   **nh**: `nh`.
-   **WH**: input array `WH`.
-   **strideWH1**: stride of dimension 1 of `WH`.
-   **strideWH2**: stride of dimension 2 of `WH`.
-   **offsetWH**: starting index for `WH`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlaqr5()` corresponds to the [LAPACK][lapack] level routine [`zlaqr5`][lapack-zlaqr5].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlaqr5 = require( '@stdlib/lapack/base/zlaqr5' );

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

[lapack-zlaqr5]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlaqr5.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->