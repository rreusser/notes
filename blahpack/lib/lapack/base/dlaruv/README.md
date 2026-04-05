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

# dlaruv

> Returns a vector of n random real numbers from a uniform (0,1) distribution.

<section class="usage">

## Usage

```javascript
var dlaruv = require( '@stdlib/lapack/base/dlaruv' );
```

#### dlaruv( iseed, strideISEED, N, x, strideX )

Returns a vector of n random real numbers from a uniform (0,1) distribution.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **iseed**: `iseed`.
-   **strideISEED**: stride length for `ISEED`.
-   **N**: number of columns.
-   **x**: `x`.
-   **strideX**: stride length for `X`.

#### dlaruv.ndarray( iseed, strideISEED, offsetISEED, N, x, strideX, offsetX )

Returns a vector of n random real numbers from a uniform (0,1) distribution, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetISEED**: starting index for `ISEED`.
-   **offsetX**: starting index for `X`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlaruv()` corresponds to the [LAPACK][lapack] level routine [`dlaruv`][lapack-dlaruv].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlaruv = require( '@stdlib/lapack/base/dlaruv' );

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

[lapack-dlaruv]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlaruv.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->