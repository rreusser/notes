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

# zgelqt3

> Recursively computes an LQ factorization of a complex M-by-N matrix using the compact WY representation of Q.

<section class="usage">

## Usage

```javascript
var zgelqt3 = require( '@stdlib/lapack/base/zgelqt3' );
```

#### zgelqt3( order, M, N, A, LDA, T, LDT )

Recursively computes an LQ factorization of a complex M-by-N matrix using the compact WY representation of Q.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

// Column-major 2-by-3 complex matrix (M=2, N=3, LDA=2):
var A = new Complex128Array( [ 2.0, 0.1, 0.5, -0.2, 1.0, 0.3, 3.0, 0.4, 0.5, -0.1, 1.5, 0.2 ] );
var T = new Complex128Array( 4 ); // 2-by-2

zgelqt3( 'column-major', 2, 3, A, 2, T, 2 );
// On exit: A holds L below the diagonal and V above; T holds the upper-triangular block reflector.
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of the matrix `A` (`M <= N`).
-   **N**: number of columns of the matrix `A`.
-   **A**: input/output `Complex128Array`; on exit contains `L` and `V`.
-   **LDA**: leading dimension of `A`.
-   **T**: output upper-triangular factor of the block reflector.
-   **LDT**: leading dimension of `T`.

#### zgelqt3.ndarray( M, N, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT )

Recursively computes an LQ factorization of a complex M-by-N matrix using the compact WY representation of Q, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var A = new Complex128Array( [ 2.0, 0.1, 0.5, -0.2, 1.0, 0.3, 3.0, 0.4, 0.5, -0.1, 1.5, 0.2 ] );
var T = new Complex128Array( 4 );

// Column-major: strideA1=1, strideA2=M (=2)
zgelqt3.ndarray( 2, 3, A, 1, 2, 0, T, 1, 2, 0 );
```

The function has the following additional parameters:

-   **M**: number of rows.
-   **N**: number of columns.
-   **A**: input matrix.
-   **strideA1**: stride of dimension 1 of `A` (in complex elements).
-   **strideA2**: stride of dimension 2 of `A` (in complex elements).
-   **offsetA**: starting index for `A` (in complex elements).
-   **T**: output matrix.
-   **strideT1**: stride of dimension 1 of `T` (in complex elements).
-   **strideT2**: stride of dimension 2 of `T` (in complex elements).
-   **offsetT**: starting index for `T` (in complex elements).

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Requires `M <= N`.
-   On exit, the elements on and below the diagonal of `A` contain the `M`-by-`M` lower triangular matrix `L`; the elements above the diagonal are the rows of `V` defining the elementary reflectors. The implicit `1`s on the diagonal of `V` are not stored.
-   On exit, the elements on and above the diagonal of `T` contain the `M`-by-`M` upper triangular block reflector factor; the elements below the diagonal are not used.
-   The block reflector is `H = I - V * T * V^H` where `V^H` denotes the conjugate transpose of `V`.
-   Based on the algorithm of Elmroth and Gustavson, _IBM J. Res. Develop._ Vol 44 No. 4, July 2000.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var zgelqt3 = require( '@stdlib/lapack/base/zgelqt3' );

var M = 3;
var N = 5;
var A = new Complex128Array( [
    4.0, 0.2,  0.5, 0.4,  0.3, -0.5,
    1.0, -0.3, 3.5, -0.2, 0.8, 0.1,
    0.5, 0.4,  1.2, 0.6,  4.5, -0.3,
    -0.25, 0.1, 0.6, -0.3, 1.1, 0.5,
    0.75, -0.5, -0.4, 0.2, 0.9, -0.2
] );
var T = new Complex128Array( M * M );

var info = zgelqt3( 'column-major', M, N, A, M, T, M );
console.log( info );
// => 0
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
