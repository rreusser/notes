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

# dtpttf

> Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).

<section class="usage">

## Usage

```javascript
var dtpttf = require( '@stdlib/lapack/base/dtpttf' );
```

#### dtpttf( transr, uplo, N, AP, ARF )

Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **transr**: `transr`.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **ARF**: input array `ARF`.

#### dtpttf.ndarray( transr, uplo, N, AP, strideAP, offsetAP, ARF, strideARF, offsetARF )

Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP), using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideARF**: stride length for `ARF`.
-   **offsetARF**: starting index for `ARF`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dtpttf()` corresponds to the [LAPACK][lapack] level routine [`dtpttf`][lapack-dtpttf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dtpttf = require( '@stdlib/lapack/base/dtpttf' );

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

[lapack-dtpttf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dtpttf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->