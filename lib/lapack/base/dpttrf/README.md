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

# dpttrf

> Computes the L_D_L^T factorization of a real symmetric positive definite tridiagonal matrix A.

<section class="usage">

## Usage

```javascript
var dpttrf = require( '@stdlib/lapack/base/dpttrf' );
```

#### dpttrf( N, d, strideD, e, strideE )

Computes the L_D_L^T factorization of a real symmetric positive definite tridiagonal matrix A.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **d**: `d`.
-   **strideD**: stride length for `D`.
-   **e**: `e`.
-   **strideE**: stride length for `E`.

#### dpttrf.ndarray( N, d, strideD, offsetD, e, strideE, offsetE )

Computes the L_D_L^T factorization of a real symmetric positive definite tridiagonal matrix A, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetD**: starting index for `D`.
-   **offsetE**: starting index for `E`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dpttrf()` corresponds to the [LAPACK][lapack] level routine [`dpttrf`][lapack-dpttrf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dpttrf = require( '@stdlib/lapack/base/dpttrf' );

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

[lapack-dpttrf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dpttrf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->