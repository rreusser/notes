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

# zlarf

> Apply a complex elementary reflector H to a complex M-by-N matrix C,.

<section class="usage">

## Usage

```javascript
var zlarf = require( '@stdlib/lapack/base/zlarf' );
```

#### zlarf( order, side, M, N, v, strideV, tau, offsetTau, C, LDC, WORK, strideWORK )

Apply a complex elementary reflector H to a complex M-by-N matrix C,.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: specifies the side of the operation.
-   **M**: number of rows.
-   **N**: number of columns.
-   **v**: `v`.
-   **strideV**: stride length for `V`.
-   **tau**: `tau`.
-   **offsetTau**: starting index for `Tau`.
-   **C**: input array `C`.
-   **LDC**: leading dimension of `C`.
-   **WORK**: input array `WORK`.
-   **strideWORK**: stride length for `WORK`.

#### zlarf.ndarray( side, M, N, v, strideV, offsetV, tau, offsetTau, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )

Apply a complex elementary reflector H to a complex M-by-N matrix C,, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetV**: starting index for `V`.
-   **strideC1**: stride of dimension 1 of `C`.
-   **strideC2**: stride of dimension 2 of `C`.
-   **offsetC**: starting index for `C`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlarf()` corresponds to the [LAPACK][lapack] level routine [`zlarf`][lapack-zlarf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlarf = require( '@stdlib/lapack/base/zlarf' );

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

[lapack-zlarf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlarf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->