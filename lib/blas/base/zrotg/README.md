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

# zrotg

> Computes the parameters for a Givens rotation.

<section class="usage">

## Usage

```javascript
var zrotg = require( '@stdlib/blas/base/zrotg' );
```

#### zrotg( re, im )

Computes the parameters for a Givens rotation.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **re**: `re`.
-   **im**: `im`.

#### zrotg.ndarray( a, offsetA, b, offsetB, c, offsetC, s, offsetS )

Computes the parameters for a Givens rotation, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **a**: `a`.
-   **offsetA**: starting index for `A`.
-   **b**: `b`.
-   **offsetB**: starting index for `B`.
-   **c**: `c`.
-   **offsetC**: starting index for `C`.
-   **s**: `s`.
-   **offsetS**: starting index for `S`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zrotg()` corresponds to the [LAPACK][lapack] level routine [`zrotg`][lapack-zrotg].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zrotg = require( '@stdlib/blas/base/zrotg' );

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

[lapack-zrotg]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zrotg.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->