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

# dlaswlq

> Computes a blocked Tall-Skinny LQ (TSLQ) factorization of a real `M`-by-`N` matrix (with `M <= N`).

<section class="usage">

## Usage

```javascript
var dlaswlq = require( '@stdlib/lapack/base/dlaswlq' );
```

#### dlaswlq( order, M, N, mb, nb, A, LDA, T, LDT, WORK )

Computes the LQ factorization `A = (L 0) * Q` of a real `M`-by-`N` short-wide matrix `A` partitioned into column blocks of size `nb`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [
    5.0, 1.0, 0.5, 0.3,
    1.0, 6.0, 1.0, 0.5,
    0.5, 1.0, 7.0, 1.0
] );
var T = new Float64Array( 6 ); // mb-by-M
var WORK = new Float64Array( 6 );

dlaswlq( 'row-major', 3, 4, 2, 8, A, 4, T, 2, WORK );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `A` (`M <= N`).
-   **N**: number of columns of `A`.
-   **mb**: inner block size (`1 <= mb <= M` when `M > 0`).
-   **nb**: column block size (`nb >= 0`).
-   **A**: input matrix overwritten on exit; the lower triangle holds `L`, and the elements above the diagonal hold the Householder reflectors `V`.
-   **LDA**: leading dimension of `A`.
-   **T**: output matrix of upper triangular block reflector factors.
-   **LDT**: leading dimension of `T` (`LDT >= mb`).
-   **WORK**: workspace array of length at least `mb*M`.

#### dlaswlq.ndarray( M, N, mb, nb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, WORK, strideWORK, offsetWORK )

Same operation as above, but using stride/offset semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array( [ 5.0, 0.5, 1.0, 6.0, 0.5, 1.0 ] );
var T = new Float64Array( 4 );
var WORK = new Float64Array( 4 );

dlaswlq.ndarray( 2, 3, 2, 8, A, 1, 2, 0, T, 1, 2, 0, WORK, 1, 0 );
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The factorization is appropriate for short-wide matrices where `N >> M`. The first column block of size `M`-by-`nb` is factored via [`dgelqt`][@stdlib/lapack/base/dgelqt]; subsequent column blocks are combined into the running `L` factor via [`dtplqt`][@stdlib/lapack/base/dtplqt].
-   `T` has dimensions `mb`-by-`(M * Number_of_row_blocks)` where `Number_of_row_blocks = ceil((N-M)/(nb-M))`. When `M >= N`, `nb <= M`, or `nb >= N`, the routine simply calls `dgelqt` and `T` is `mb`-by-`M`.
-   The orthogonal factor `Q` is represented implicitly by the Householder reflectors stored above the diagonal of `A` together with `T`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlaswlq = require( '@stdlib/lapack/base/dlaswlq' );

var M = 3;
var N = 8;
var mb = 2;
var nb = 4;
var A = new Float64Array( [
    5.0, 0.5, 0.333,
    0.5, 6.0, 0.5,
    0.333, 0.5, 7.0,
    0.25, 0.333, 0.5,
    0.2, 0.25, 0.333,
    0.167, 0.2, 0.25,
    0.143, 0.167, 0.2,
    0.125, 0.143, 0.167
] );
var T = new Float64Array( mb * M * 5 );
var WORK = new Float64Array( mb * M );

var info = dlaswlq( 'column-major', M, N, mb, nb, A, M, T, mb, WORK );
console.log( info );
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

[@stdlib/lapack/base/dgelqt]: https://github.com/stdlib-js/stdlib

[@stdlib/lapack/base/dtplqt]: https://github.com/stdlib-js/stdlib

</section>

<!-- /.links -->
