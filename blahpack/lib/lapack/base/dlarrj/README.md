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

# dlarrj

> Refine eigenvalue approximations using bisection given initial intervals.

<section class="usage">

## Usage

```javascript
var dlarrj = require( '@stdlib/lapack/base/dlarrj' );
```

#### dlarrj( N, d, strideD, E2, strideE2, ifirst, ilast, rtol, offset, w, strideW, WERR, strideWERR, WORK, strideWORK, IWORK, strideIWORK, pivmin, spdiam )

Refine eigenvalue approximations using bisection given initial intervals.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **d**: `d`.
-   **strideD**: stride length for `D`.
-   **E2**: input array `E2`.
-   **strideE2**: stride of dimension 2 of `E`.
-   **ifirst**: `ifirst`.
-   **ilast**: `ilast`.
-   **rtol**: `rtol`.
-   **offset**: `offset`.
-   **w**: `w`.
-   **strideW**: stride length for `W`.
-   **WERR**: input array `WERR`.
-   **strideWERR**: stride length for `WERR`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.
-   **IWORK**: input array `IWORK`.
-   **strideIWORK**: stride length for `IWORK`.
-   **pivmin**: `pivmin`.
-   **spdiam**: `spdiam`.

#### dlarrj.ndarray( N, d, strideD, offsetD, E2, strideE2, offsetE2, ifirst, ilast, rtol, offset, w, strideW, offsetW, WERR, strideWERR, offsetWERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK, pivmin, spdiam )

Refine eigenvalue approximations using bisection given initial intervals, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetD**: starting index for `D`.
-   **offsetE2**: starting index for `E2`.
-   **offsetW**: starting index for `W`.
-   **offsetWERR**: starting index for `WERR`.
-   **offsetWORK**: starting index for `WORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlarrj()` corresponds to the [LAPACK][lapack] level routine [`dlarrj`][lapack-dlarrj].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlarrj = require( '@stdlib/lapack/base/dlarrj' );

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

[lapack-dlarrj]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlarrj.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->