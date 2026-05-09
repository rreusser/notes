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

# dgelqt

> Compute a blocked LQ factorization of a real M-by-N matrix A using the compact WY representation of Q.

<section class="usage">

## Usage

```javascript
var dgelqt = require( '@stdlib/lapack/base/dgelqt' );
```

#### dgelqt( order, M, N, mb, A, LDA, T, LDT, WORK )

Compute a blocked LQ factorization of a real `M`-by-`N` matrix `A` using the compact WY representation of `Q`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([ 3.0, 0.5, 0.6, 4.0, 0.4, 0.7, 0.2, 0.3 ]);
var T = new Float64Array( 4 ); // mb*K = 2*2
var WORK = new Float64Array( 8 ); // mb*N = 2*4

var info = dgelqt( 'column-major', 2, 4, 2, A, 2, T, 2, WORK );
// info => 0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **mb**: block size; must satisfy `1 <= mb <= min(M,N)` (when `min(M,N) > 0`).
-   **A**: input/output [`Float64Array`][mdn-float64array]. On exit, the elements on and below the diagonal contain `L`; the elements above the diagonal are the rows of `V`.
-   **LDA**: leading dimension of `A`.
-   **T**: output [`Float64Array`][mdn-float64array] of shape `mb`-by-`min(M,N)` storing the upper triangular block reflectors.
-   **LDT**: leading dimension of `T`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length `mb*N`.

#### dgelqt.ndarray( M, N, mb, A, strideA1, strideA2, offsetA, T, strideT1, strideT2, offsetT, WORK, offsetWORK )

Compute a blocked LQ factorization using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var A = new Float64Array([ 3.0, 0.5, 0.6, 4.0, 0.4, 0.7, 0.2, 0.3 ]);
var T = new Float64Array( 4 );
var WORK = new Float64Array( 8 );

var info = dgelqt.ndarray( 2, 4, 2, A, 1, 2, 0, T, 1, 2, 0, WORK, 0 );
// info => 0
```

The function has the following additional parameters:

-   **strideA1**: stride of dimension 1 of `A`.
-   **strideA2**: stride of dimension 2 of `A`.
-   **offsetA**: starting index for `A`.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dgelqt` calls the recursive panel kernel `dgelqt3` and applies each panel's block reflector to the trailing matrix via `dlarfb` (side `'right'`, no-transpose, forward, rowwise storage).
-   Unlike the Fortran reference, `mb` is exposed as an explicit parameter (no `ILAENV` query). Pass `mb = 32` for a typical default.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dgelqt = require( '@stdlib/lapack/base/dgelqt' );

var M = 4;
var N = 6;
var mb = 2;

var A = new Float64Array([
    3.0, 0.5, 0.2, 0.4,
    0.6, 4.0, 0.5, 0.3,
    0.4, 0.7, 3.5, 0.5,
    0.2, 0.3, 0.8, 4.5,
    0.1, -0.2, 0.6, 1.1,
    -0.3, 0.5, 0.1, -0.5
]);
var T = new Float64Array( mb * Math.min( M, N ) );
var WORK = new Float64Array( mb * N );

var info = dgelqt( 'column-major', M, N, mb, A, M, T, mb, WORK );
console.log( info );
console.log( A );
console.log( T );
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

</section>

<!-- /.links -->
