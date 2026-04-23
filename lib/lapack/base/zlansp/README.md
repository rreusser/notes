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

# zlansp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage.

<section class="usage">

## Usage

```javascript
var zlansp = require( '@stdlib/lapack/base/zlansp' );
```

#### zlansp( norm, uplo, N, AP, WORK )

Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **norm**: `norm`.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **WORK**: input array `WORK`.

#### zlansp.ndarray( norm, uplo, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix supplied in packed storage, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlansp()` corresponds to the [LAPACK][lapack] level routine [`zlansp`][lapack-zlansp].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlansp = require( '@stdlib/lapack/base/zlansp' );

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

[lapack-zlansp]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlansp.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->