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

# izmax1

> Finds the index of the first vector element of maximum absolute value.

<section class="usage">

## Usage

```javascript
var izmax1 = require( '@stdlib/lapack/base/izmax1' );
```

#### izmax1( N, ZX, strideZX )

Finds the index of the first vector element of maximum absolute value.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **ZX**: input array `ZX`.
-   **strideZX**: stride length for `ZX`.

#### izmax1.ndarray( N, ZX, strideZX, offsetZX )

Finds the index of the first vector element of maximum absolute value, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetZX**: starting index for `ZX`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `izmax1()` corresponds to the [LAPACK][lapack] level routine [`izmax1`][lapack-izmax1].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var izmax1 = require( '@stdlib/lapack/base/izmax1' );

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

[lapack-izmax1]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__izmax1.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->