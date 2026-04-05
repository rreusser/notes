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

# zlapmr

> Rearranges the rows of an M-by-N complex matrix X as specified by a permutation vector.

<section class="usage">

## Usage

```javascript
var zlapmr = require( '@stdlib/lapack/base/zlapmr' );
```

#### zlapmr( forwrd, M, N, X, LDX, k, strideK )

Rearranges the rows of an M-by-N complex matrix X as specified by a permutation vector.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **forwrd**: `forwrd`.
-   **M**: number of rows.
-   **N**: number of columns.
-   **X**: input array `X`.
-   **LDX**: leading dimension of `X`.
-   **k**: `k`.
-   **strideK**: stride length for `K`.

#### zlapmr.ndarray( forwrd, M, N, X, strideX1, strideX2, offsetX, k, strideK, offsetK )

Rearranges the rows of an M-by-N complex matrix X as specified by a permutation vector, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideX1**: stride of dimension 1 of `X`.
-   **strideX2**: stride of dimension 2 of `X`.
-   **offsetX**: starting index for `X`.
-   **offsetK**: starting index for `K`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlapmr()` corresponds to the [LAPACK][lapack] level routine [`zlapmr`][lapack-zlapmr].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlapmr = require( '@stdlib/lapack/base/zlapmr' );

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

[lapack-zlapmr]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlapmr.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->