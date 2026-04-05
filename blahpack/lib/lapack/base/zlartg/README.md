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

# zlartg

> Compute |re|^2 + |im|^2.

<section class="usage">

## Usage

```javascript
var zlartg = require( '@stdlib/lapack/base/zlartg' );
```

#### zlartg( f, offsetF, g, offsetG, c, offsetC, s, offsetS, r, offsetR )

Compute |re|^2 + |im|^2.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **f**: `f`.
-   **offsetF**: starting index for `F`.
-   **g**: `g`.
-   **offsetG**: starting index for `G`.
-   **c**: `c`.
-   **offsetC**: starting index for `C`.
-   **s**: `s`.
-   **offsetS**: starting index for `S`.
-   **r**: `r`.
-   **offsetR**: starting index for `R`.

#### zlartg.ndarray( f, offsetF, g, offsetG, c, offsetC, s, offsetS, r, offsetR )

Compute |re|^2 + |im|^2, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:


</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlartg()` corresponds to the [LAPACK][lapack] level routine [`zlartg`][lapack-zlartg].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlartg = require( '@stdlib/lapack/base/zlartg' );

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

[lapack-zlartg]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlartg.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->