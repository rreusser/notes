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

# zlar2v

> Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.

<section class="usage">

## Usage

```javascript
var zlar2v = require( '@stdlib/lapack/base/zlar2v' );
```

#### zlar2v( N, x, y, z, strideXYZ, c, s, strideCS )

Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **x**: `x`.
-   **y**: `y`.
-   **z**: `z`.
-   **strideXYZ**: stride length for `XYZ`.
-   **c**: `c`.
-   **s**: `s`.
-   **strideCS**: stride length for `CS`.

#### zlar2v.ndarray( N, x, strideX, offsetX, y, strideY, offsetY, z, strideZ, offsetZ, c, strideC, offsetC, s, strideS, offsetS )

Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideX**: stride length for `X`.
-   **offsetX**: starting index for `X`.
-   **strideY**: stride length for `Y`.
-   **offsetY**: starting index for `Y`.
-   **strideZ**: stride length for `Z`.
-   **offsetZ**: starting index for `Z`.
-   **strideC**: stride length for `C`.
-   **offsetC**: starting index for `C`.
-   **strideS**: stride length for `S`.
-   **offsetS**: starting index for `S`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlar2v()` corresponds to the [LAPACK][lapack] level routine [`zlar2v`][lapack-zlar2v].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlar2v = require( '@stdlib/lapack/base/zlar2v' );

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

[lapack-zlar2v]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlar2v.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->