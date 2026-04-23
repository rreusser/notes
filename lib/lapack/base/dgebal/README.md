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

# dgebal

> Balances a general real matrix A.

<section class="usage">

## Usage

```javascript
var dgebal = require( '@stdlib/lapack/base/dgebal' );
```

#### dgebal( job, N, A, LDA, SCALE, strideSCALE )

Balances a general real matrix A.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: `job`.
-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **SCALE**: input array `SCALE`.
-   **strideSCALE**: stride length for `SCALE`.

#### dgebal.ndarray( job, N, A, strideA1, strideA2, offsetA, ilo, ihi, SCALE, strideSCALE, offsetSCALE )

Balances a general real matrix A, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **ilo**: `ilo`.
-   **ihi**: `ihi`.
-   **offsetSCALE**: starting index for `SCALE`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgebal()` corresponds to the [LAPACK][lapack] level routine [`dgebal`][lapack-dgebal].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dgebal = require( '@stdlib/lapack/base/dgebal' );

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

[lapack-dgebal]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dgebal.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->