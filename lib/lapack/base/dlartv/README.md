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

# dlartv

> Applies a vector of real plane rotations to elements of two real vectors.

<section class="usage">

## Usage

```javascript
var dlartv = require( '@stdlib/lapack/base/dlartv' );
```

#### dlartv( N, x, strideX, y, strideY, c, s, strideCS )

Applies a vector of real plane rotations to elements of two real vectors.

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
-   **s**: `s`.
-   **strideCS**: stride length for `CS`.

#### dlartv.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, c, strideC, offsetC, s, strideS, offsetS )

Applies a vector of real plane rotations to elements of two real vectors, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetX**: starting index for `X`.
-   **offsetY**: starting index for `Y`.
-   **strideC**: stride length for `C`.
-   **offsetC**: starting index for `C`.
-   **strideS**: stride length for `S`.
-   **offsetS**: starting index for `S`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlartv()` corresponds to the [LAPACK][lapack] level routine [`dlartv`][lapack-dlartv].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlartv = require( '@stdlib/lapack/base/dlartv' );

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

[lapack-dlartv]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlartv.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->