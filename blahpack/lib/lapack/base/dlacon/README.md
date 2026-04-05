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

# dlacon

> @license Apache-2.0.

<section class="usage">

## Usage

```javascript
var dlacon = require( '@stdlib/lapack/base/dlacon' );
```

#### dlacon( N, v, strideV, x, strideX, ISGN, strideISGN, EST, KASE )

@license Apache-2.0.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **v**: `v`.
-   **strideV**: stride length for `V`.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **ISGN**: input array `ISGN`.
-   **strideISGN**: stride length for `ISGN`.
-   **EST**: input array `EST`.
-   **KASE**: input array `KASE`.

#### dlacon.ndarray( N, v, strideV, offsetV, x, strideX, offsetX, ISGN, strideISGN, offsetISGN, EST, KASE )

@license Apache-2.0, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetV**: starting index for `V`.
-   **offsetX**: starting index for `X`.
-   **offsetISGN**: starting index for `ISGN`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlacon()` corresponds to the [LAPACK][lapack] level routine [`dlacon`][lapack-dlacon].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlacon = require( '@stdlib/lapack/base/dlacon' );

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

[lapack-dlacon]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlacon.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->