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

# dlaqr0

> Compute the number of simultaneous shifts (ISPEC=15) per iparmq.

<section class="usage">

## Usage

```javascript
var dlaqr0 = require( '@stdlib/lapack/base/dlaqr0' );
```

#### dlaqr0( nh )

Compute the number of simultaneous shifts (ISPEC=15) per iparmq.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **nh**: `nh`.

#### dlaqr0.ndarray( wantt, wantz, N, ilo, ihi, H, strideH1, strideH2, offsetH, WR, strideWR, offsetWR, WI, strideWI, offsetWI, iloz, ihiz, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork )

Compute the number of simultaneous shifts (ISPEC=15) per iparmq, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **wantt**: `wantt`.
-   **wantz**: `wantz`.
-   **N**: number of columns.
-   **ilo**: `ilo`.
-   **ihi**: `ihi`.
-   **H**: input array `H`.
-   **strideH1**: stride of dimension 1 of `H`.
-   **strideH2**: stride of dimension 2 of `H`.
-   **offsetH**: starting index for `H`.
-   **WR**: input array `WR`.
-   **strideWR**: stride length for `WR`.
-   **offsetWR**: starting index for `WR`.
-   **WI**: input array `WI`.
-   **strideWI**: stride length for `WI`.
-   **offsetWI**: starting index for `WI`.
-   **iloz**: `iloz`.
-   **ihiz**: `ihiz`.
-   **Z**: input array `Z`.
-   **strideZ1**: stride of dimension 1 of `Z`.
-   **strideZ2**: stride of dimension 2 of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: `lwork`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlaqr0()` corresponds to the [LAPACK][lapack] level routine [`dlaqr0`][lapack-dlaqr0].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlaqr0 = require( '@stdlib/lapack/base/dlaqr0' );

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

[lapack-dlaqr0]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlaqr0.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->