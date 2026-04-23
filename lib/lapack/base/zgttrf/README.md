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

# zgttrf

> CABS1: |re(z)| + |im(z)|.

<section class="usage">

## Usage

```javascript
var zgttrf = require( '@stdlib/lapack/base/zgttrf' );
```

#### zgttrf( v, idx )

CABS1: |re(z)| + |im(z)|.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **v**: `v`.
-   **idx**: `idx`.

#### zgttrf.ndarray( N, DL, strideDL, offsetDL, d, strideD, offsetD, DU, strideDU, offsetDU, DU2, strideDU2, offsetDU2, IPIV, strideIPIV, offsetIPIV )

CABS1: |re(z)| + |im(z)|, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **N**: number of columns.
-   **DL**: input array `DL`.
-   **strideDL**: stride length for `DL`.
-   **offsetDL**: starting index for `DL`.
-   **d**: `d`.
-   **strideD**: stride length for `D`.
-   **offsetD**: starting index for `D`.
-   **DU**: input array `DU`.
-   **strideDU**: stride length for `DU`.
-   **offsetDU**: starting index for `DU`.
-   **DU2**: input array `DU2`.
-   **strideDU2**: stride of dimension 2 of `DU`.
-   **offsetDU2**: starting index for `DU2`.
-   **IPIV**: input array `IPIV`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgttrf()` corresponds to the [LAPACK][lapack] level routine [`zgttrf`][lapack-zgttrf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zgttrf = require( '@stdlib/lapack/base/zgttrf' );

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

[lapack-zgttrf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zgttrf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->