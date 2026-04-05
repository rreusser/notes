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

# zlasr

> Applies a sequence of real plane rotations to a complex general matrix.

<section class="usage">

## Usage

```javascript
var zlasr = require( '@stdlib/lapack/base/zlasr' );
```

#### zlasr( order, side, pivot, direct, M, N, c, strideC, s, strideS, A, LDA )

Applies a sequence of real plane rotations to a complex general matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **side**: specifies the side of the operation.
-   **pivot**: `pivot`.
-   **direct**: `direct`.
-   **M**: number of rows.
-   **N**: number of columns.
-   **c**: `c`.
-   **strideC**: stride length for `C`.
-   **s**: `s`.
-   **strideS**: stride length for `S`.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.

#### zlasr.ndarray( side, pivot, direct, M, N, c, strideC, offsetC, s, strideS, offsetS, A, strideA1, strideA2, offsetA )

Applies a sequence of real plane rotations to a complex general matrix, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetC**: starting index for `C`.
-   **offsetS**: starting index for `S`.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlasr()` corresponds to the [LAPACK][lapack] level routine [`zlasr`][lapack-zlasr].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var zlasr = require( '@stdlib/lapack/base/zlasr' );

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

[lapack-zlasr]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zlasr.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->