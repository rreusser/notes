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

# dlaed6

> Computes the positive or negative root (closest to the origin) of the secular equation.

<section class="usage">

## Usage

```javascript
var dlaed6 = require( '@stdlib/lapack/base/dlaed6' );
```

#### dlaed6( kniter, orgati, rho, d, z, finit, tau )

Computes the positive or negative root (closest to the origin) of the secular equation.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **kniter**: `kniter`.
-   **orgati**: `orgati`.
-   **rho**: `rho`.
-   **d**: `d`.
-   **z**: `z`.
-   **finit**: `finit`.
-   **tau**: `tau`.

#### dlaed6.ndarray( kniter, orgati, rho, d, strideD, offsetD, z, strideZ, offsetZ, finit, tau )

Computes the positive or negative root (closest to the origin) of the secular equation, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideD**: stride length for `D`.
-   **offsetD**: starting index for `D`.
-   **strideZ**: stride length for `Z`.
-   **offsetZ**: starting index for `Z`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlaed6()` corresponds to the [LAPACK][lapack] level routine [`dlaed6`][lapack-dlaed6].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlaed6 = require( '@stdlib/lapack/base/dlaed6' );

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

[lapack-dlaed6]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlaed6.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->