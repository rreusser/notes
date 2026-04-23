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

# zlahr2

> Reduces the first nb columns of a complex general rectangular matrix to upper trapezoidal form.

<section class="usage">

## Usage

```javascript
var zlahr2 = require( '@stdlib/lapack/base/zlahr2' );
```

#### zlahr2( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY )

Reduces the first nb columns of a complex general rectangular matrix to upper trapezoidal form.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **K**: inner dimension.
-   **nb**: `nb`.
-   **A**: input array `A`.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **tau**: `tau`.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **T**: input array `T`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **Y**: input array `Y`.
-   **strideY1**: stride of dimension 1 of `Y`.
-   **strideY2**: stride of dimension 2 of `Y`.
-   **offsetY**: starting index for `Y`.

#### zlahr2.ndarray( N, K, nb, A, strideA1, strideA2, offsetA, tau, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT, Y, strideY1, strideY2, offsetY )

Reduces the first nb columns of a complex general rectangular matrix to upper trapezoidal form, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:


</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlahr2()` corresponds to the [LAPACK][lapack] level routine [`zlahr2`][lapack-zlahr2].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlahr2 = require( '@stdlib/lapack/base/zlahr2' );

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

[lapack-zlahr2]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlahr2.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->