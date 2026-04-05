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

# dlasd5

> Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.

<section class="usage">

## Usage

```javascript
var dlasd5 = require( '@stdlib/lapack/base/dlasd5' );
```

#### dlasd5( i, D, strideD, Z, strideZ, DELTA, strideDELTA, rho, dsigma, WORK, strideWORK )

Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **i**: `i`.
-   **D**: input array `D`.
-   **strideD**: stride length for `D`.
-   **Z**: input array `Z`.
-   **strideZ**: stride length for `Z`.
-   **DELTA**: input array `DELTA`.
-   **strideDELTA**: stride length for `DELTA`.
-   **rho**: `rho`.
-   **dsigma**: `dsigma`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.

#### dlasd5.ndarray( i, d, strideD, offsetD, z, strideZ, offsetZ, DELTA, strideDELTA, offsetDELTA, rho, dsigma, WORK, strideWORK, offsetWORK )

Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **d**: `d`.
-   **offsetD**: starting index for `D`.
-   **z**: `z`.
-   **offsetZ**: starting index for `Z`.
-   **offsetDELTA**: starting index for `DELTA`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlasd5()` corresponds to the [LAPACK][lapack] level routine [`dlasd5`][lapack-dlasd5].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlasd5 = require( '@stdlib/lapack/base/dlasd5' );

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

[lapack-dlasd5]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlasd5.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->