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

# zlasyf_rk

> ZLASYF_RK computes a partial factorization of a complex symmetric indefinite matrix using bounded Bunch-Kaufman (rook) diagonal pivoting method, producing _rk format output

<section class="usage">

## Usage

```javascript
var zlasyf_rk = require( '@stdlib/lapack/base/zlasyf_rk' );
```

#### zlasyf_rk( order, uplo, N, nb, kb, A, LDA, e, strideE, IPIV, strideIPIV, offsetIPIV, W, LDW )

ZLASYF_RK computes a partial factorization of a complex symmetric indefinite matrix using bounded Bunch-Kaufman (rook) diagonal pivoting method, producing _rk format output

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **nb**: nb.
-   **kb**: kb.
-   **A**: input matrix.
-   **LDA**: leading dimension of `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **W**: output matrix.
-   **LDW**: leading dimension of `W`.

#### zlasyf_rk.ndarray( uplo, N, nb, kb, A, strideA1, strideA2, offsetA, e, strideE, offsetE, IPIV, strideIPIV, offsetIPIV, W, strideW1, strideW2, offsetW )

ZLASYF_RK computes a partial factorization of a complex symmetric indefinite matrix using bounded Bunch-Kaufman (rook) diagonal pivoting method, producing _rk format output, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **uplo**: specifies the operation type.
-   **N**: number of columns.
-   **nb**: nb.
-   **kb**: kb.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `E`.
-   **IPIV**: input array.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **W**: output matrix.
-   **strideW1**: stride of dimension 1 of `W`.
-   **strideW2**: stride of dimension 2 of `W`.
-   **offsetW**: starting index for `W`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
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

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
