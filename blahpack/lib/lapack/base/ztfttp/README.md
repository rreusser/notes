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

# ztfttp

> Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).

<section class="usage">

## Usage

```javascript
var ztfttp = require( '@stdlib/lapack/base/ztfttp' );
```

#### ztfttp( transr, uplo, N, ARF, AP )

Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **transr**: `transr`.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **ARF**: input array `ARF`.
-   **AP**: input array `AP`.

#### ztfttp.ndarray( transr, uplo, N, ARF, strideARF, offsetARF, AP, strideAP, offsetAP )

Copy a complex triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP), using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideARF**: stride length for `ARF`.
-   **offsetARF**: starting index for `ARF`.
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `ztfttp()` corresponds to the [LAPACK][lapack] level routine [`ztfttp`][lapack-ztfttp].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var ztfttp = require( '@stdlib/lapack/base/ztfttp' );

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

[lapack-ztfttp]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__ztfttp.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->