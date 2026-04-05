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

# zpptri

> Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.

<section class="usage">

## Usage

```javascript
var zpptri = require( '@stdlib/lapack/base/zpptri' );
```

#### zpptri( uplo, N, AP )

Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **AP**: input array `AP`.

#### zpptri.ndarray( uplo, N, AP, stride, offset )

Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **stride**: `stride`.
-   **offset**: `offset`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zpptri()` corresponds to the [LAPACK][lapack] level routine [`zpptri`][lapack-zpptri].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zpptri = require( '@stdlib/lapack/base/zpptri' );

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

[lapack-zpptri]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zpptri.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->