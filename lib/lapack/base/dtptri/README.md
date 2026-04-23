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

# dtptri

> Computes the inverse of a real upper or lower triangular matrix in packed storage.

<section class="usage">

## Usage

```javascript
var dtptri = require( '@stdlib/lapack/base/dtptri' );
```

#### dtptri( uplo, diag, N, AP )

Computes the inverse of a real upper or lower triangular matrix in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **diag**: specifies whether the matrix is unit triangular.
-   **N**: number of columns.
-   **AP**: input array `AP`.

#### dtptri.ndarray( uplo, diag, N, AP, strideAP, offsetAP )

Computes the inverse of a real upper or lower triangular matrix in packed storage, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dtptri()` corresponds to the [LAPACK][lapack] level routine [`dtptri`][lapack-dtptri].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dtptri = require( '@stdlib/lapack/base/dtptri' );

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

[lapack-dtptri]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dtptri.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->