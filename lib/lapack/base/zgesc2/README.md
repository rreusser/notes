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

# zgesc2

> Solves a system of linear equations `A * X = scale * RHS` with a general.

<section class="usage">

## Usage

```javascript
var zgesc2 = require( '@stdlib/lapack/base/zgesc2' );
```

#### zgesc2( N, A, LDA, RHS, strideRHS, IPIV, strideIPIV, JPIV, strideJPIV, scale )

Solves a system of linear equations `A * X = scale * RHS` with a general.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **RHS**: input array `RHS`.
-   **strideRHS**: stride length for `RHS`.
-   **IPIV**: input array `IPIV`.
-   **strideIPIV**: stride length for `IPIV`.
-   **JPIV**: input array `JPIV`.
-   **strideJPIV**: stride length for `JPIV`.
-   **scale**: `scale`.

#### zgesc2.ndarray( N, A, strideA1, strideA2, offsetA, RHS, strideRHS, offsetRHS, IPIV, strideIPIV, offsetIPIV, JPIV, strideJPIV, offsetJPIV, scale )

Solves a system of linear equations `A * X = scale * RHS` with a general, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetRHS**: starting index for `RHS`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **offsetJPIV**: starting index for `JPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgesc2()` corresponds to the [LAPACK][lapack] level routine [`zgesc2`][lapack-zgesc2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zgesc2 = require( '@stdlib/lapack/base/zgesc2' );

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

[lapack-zgesc2]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zgesc2.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->