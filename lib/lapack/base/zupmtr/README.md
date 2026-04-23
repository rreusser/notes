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

# zupmtr

> Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd.

<section class="usage">

## Usage

```javascript
var zupmtr = require( '@stdlib/lapack/base/zupmtr' );
```

#### zupmtr( side, uplo, trans, M, N, AP, TAU, C, LDC, WORK )

Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **side**: specifies the side of the operation.
-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **trans**: specifies whether the matrix should be transposed.
-   **M**: number of rows.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **TAU**: input array `TAU`.
-   **C**: input array `C`.
-   **LDC**: leading dimension of `C`.
-   **WORK**: input array `WORK`.

#### zupmtr.ndarray( side, uplo, trans, M, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK )

Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **strideC1**: stride of dimension 1 of `C`.
-   **strideC2**: stride of dimension 2 of `C`.
-   **offsetC**: starting index for `C`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zupmtr()` corresponds to the [LAPACK][lapack] level routine [`zupmtr`][lapack-zupmtr].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zupmtr = require( '@stdlib/lapack/base/zupmtr' );

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

[lapack-zupmtr]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zupmtr.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->