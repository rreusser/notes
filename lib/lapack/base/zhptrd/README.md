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

# zhptrd

> Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.

<section class="usage">

## Usage

```javascript
var zhptrd = require( '@stdlib/lapack/base/zhptrd' );
```

#### zhptrd( uplo, N, AP, d, e, TAU )

Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **d**: `d`.
-   **e**: `e`.
-   **TAU**: input array `TAU`.

#### zhptrd.ndarray( uplo, N, AP, strideAP, offsetAP, d, strideD, offsetD, e, strideE, offsetE, TAU, strideTAU, offsetTAU )

Reduces a complex Hermitian matrix stored in packed form to real symmetric tridiagonal form, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideD**: stride length for `D`.
-   **offsetD**: starting index for `D`.
-   **strideE**: stride length for `E`.
-   **offsetE**: starting index for `E`.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhptrd()` corresponds to the [LAPACK][lapack] level routine [`zhptrd`][lapack-zhptrd].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zhptrd = require( '@stdlib/lapack/base/zhptrd' );

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

[lapack-zhptrd]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zhptrd.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->