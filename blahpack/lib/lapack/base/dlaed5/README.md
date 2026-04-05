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

# dlaed5

> Solves the 2-by-2 secular equation.

<section class="usage">

## Usage

```javascript
var dlaed5 = require( '@stdlib/lapack/base/dlaed5' );
```

#### dlaed5( i, D, Z, DELTA, rho, dlam )

Solves the 2-by-2 secular equation.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **i**: `i`.
-   **D**: input array `D`.
-   **Z**: input array `Z`.
-   **DELTA**: input array `DELTA`.
-   **rho**: `rho`.
-   **dlam**: `dlam`.

#### dlaed5.ndarray( i, D, strideD, offsetD, Z, strideZ, offsetZ, DELTA, strideDELTA, offsetDELTA, rho, dlam )

Solves the 2-by-2 secular equation, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideD**: stride length for `D`.
-   **offsetD**: starting index for `D`.
-   **strideZ**: stride length for `Z`.
-   **offsetZ**: starting index for `Z`.
-   **strideDELTA**: stride length for `DELTA`.
-   **offsetDELTA**: starting index for `DELTA`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlaed5()` corresponds to the [LAPACK][lapack] level routine [`dlaed5`][lapack-dlaed5].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlaed5 = require( '@stdlib/lapack/base/dlaed5' );

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

[lapack-dlaed5]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlaed5.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->