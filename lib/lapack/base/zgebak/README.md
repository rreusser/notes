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

# zgebak

> Back-transforms eigenvectors after balancing by zgebal.

<section class="usage">

## Usage

```javascript
var zgebak = require( '@stdlib/lapack/base/zgebak' );
```

#### zgebak( job, side, N, ilo, ihi, SCALE, strideSCALE, M, V, LDV )

Back-transforms eigenvectors after balancing by zgebal.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: `job`.
-   **side**: specifies the side of the operation.
-   **N**: number of columns.
-   **ilo**: `ilo`.
-   **ihi**: `ihi`.
-   **SCALE**: input array `SCALE`.
-   **strideSCALE**: stride length for `SCALE`.
-   **M**: number of rows.
-   **V**: input array `V`.
-   **LDV**: leading dimension of `V`.

#### zgebak.ndarray( job, side, N, ilo, ihi, SCALE, strideSCALE, offsetSCALE, M, V, strideV1, strideV2, offsetV )

Back-transforms eigenvectors after balancing by zgebal, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetSCALE**: starting index for `SCALE`.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zgebak()` corresponds to the [LAPACK][lapack] level routine [`zgebak`][lapack-zgebak].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zgebak = require( '@stdlib/lapack/base/zgebak' );

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

[lapack-zgebak]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zgebak.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->