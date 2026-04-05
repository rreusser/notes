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

# dla_wwaddw

> Add a vector into a doubled-single accumulation vector.

<section class="usage">

## Usage

```javascript
var dla_wwaddw = require( '@stdlib/lapack/base/dla_wwaddw' );
```

#### dla_wwaddw( N, x, y, w )

Add a vector into a doubled-single accumulation vector.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **x**: `x`.
-   **y**: `y`.
-   **w**: `w`.

#### dla_wwaddw.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, w, strideW, offsetW )

Add a vector into a doubled-single accumulation vector, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideX**: stride length for `X`.
-   **offsetX**: starting index for `X`.
-   **strideY**: stride length for `Y`.
-   **offsetY**: starting index for `Y`.
-   **strideW**: stride length for `W`.
-   **offsetW**: starting index for `W`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dla_wwaddw()` corresponds to the [LAPACK][lapack] level routine [`dla_wwaddw`][lapack-dla_wwaddw].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dla_wwaddw = require( '@stdlib/lapack/base/dla_wwaddw' );

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

[lapack-dla_wwaddw]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dla_wwaddw.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->