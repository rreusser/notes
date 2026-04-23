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

# dasum

> Computes the sum of absolute values of a double-precision floating-point vector.

<section class="usage">

## Usage

```javascript
var dasum = require( '@stdlib/blas/base/dasum' );
```

#### dasum( N, x, stride )

Computes the sum of absolute values of a double-precision floating-point vector.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **x**: `x`.
-   **stride**: `stride`.

#### dasum.ndarray( N, x, stride, offset )

Computes the sum of absolute values of a double-precision floating-point vector, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offset**: `offset`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dasum()` corresponds to the [LAPACK][lapack] level routine [`dasum`][lapack-dasum].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dasum = require( '@stdlib/blas/base/dasum' );

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

[lapack-dasum]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dasum.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->