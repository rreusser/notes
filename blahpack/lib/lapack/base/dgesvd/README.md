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

# dgesvd

> Computes the singular value decomposition (SVD) of a real M-by-N matrix A,.

<section class="usage">

## Usage

```javascript
var dgesvd = require( '@stdlib/lapack/base/dgesvd' );
```

#### dgesvd( order, jobu, jobvt, M, N, A, LDA, s, strideS, U, LDU, VT, LDVT )

Computes the singular value decomposition (SVD) of a real M-by-N matrix A,.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **jobu**: `jobu`.
-   **jobvt**: `jobvt`.
-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input array `A`.
-   **LDA**: leading dimension of `A`.
-   **s**: `s`.
-   **strideS**: stride length for `S`.
-   **U**: input array `U`.
-   **LDU**: leading dimension of `U`.
-   **VT**: input array `VT`.
-   **LDVT**: leading dimension of `VT`.

#### dgesvd.ndarray( jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT )

Computes the singular value decomposition (SVD) of a real M-by-N matrix A,, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **offsetS**: starting index for `S`.
-   **strideU1**: stride of dimension 1 of `U`.
-   **strideU2**: stride of dimension 2 of `U`.
-   **offsetU**: starting index for `U`.
-   **strideVT1**: stride of dimension 1 of `VT`.
-   **strideVT2**: stride of dimension 2 of `VT`.
-   **offsetVT**: starting index for `VT`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgesvd()` corresponds to the [LAPACK][lapack] level routine [`dgesvd`][lapack-dgesvd].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dgesvd = require( '@stdlib/lapack/base/dgesvd' );

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

[lapack-dgesvd]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dgesvd.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->