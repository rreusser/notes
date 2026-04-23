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

# dlargv

> Generates a vector of real plane rotations (Givens rotations).

<section class="usage">

## Usage

```javascript
var dlargv = require( '@stdlib/lapack/base/dlargv' );
```

#### dlargv( N, x, strideX, y, strideY, c, strideC )

Generates a vector of real plane rotations (Givens rotations).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **x**: `x`.
-   **strideX**: stride length for `X`.
-   **y**: `y`.
-   **strideY**: stride length for `Y`.
-   **c**: `c`.
-   **strideC**: stride length for `C`.

#### dlargv.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, c, strideC, offsetC )

Generates a vector of real plane rotations (Givens rotations), using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetX**: starting index for `X`.
-   **offsetY**: starting index for `Y`.
-   **offsetC**: starting index for `C`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlargv()` corresponds to the [LAPACK][lapack] level routine [`dlargv`][lapack-dlargv].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlargv = require( '@stdlib/lapack/base/dlargv' );

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

[lapack-dlargv]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlargv.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->