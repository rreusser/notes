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

# dhgeqz

> Implements the QZ iteration for a real Hessenberg-triangular matrix pair (H,T), computing generalized eigenvalues and optionally the generalized Schur form.

<section class="usage">

## Usage

```javascript
var dhgeqz = require( '@stdlib/lapack/base/dhgeqz' );
```

#### dhgeqz.ndarray( job, compq, compz, N, ilo, ihi, H, strideH1, strideH2, offsetH, T, strideT1, strideT2, offsetT, ALPHAR, strideALPHAR, offsetALPHAR, ALPHAI, strideALPHAI, offsetALPHAI, BETA, strideBETA, offsetBETA, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork )

Implements the QZ iteration for a real Hessenberg-triangular matrix pair (H,T), computing generalized eigenvalues and optionally the generalized Schur form, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var H = new Float64Array( [ 2.0, 3.0, 1.5, 4.0 ] );
var T = new Float64Array( [ 1.0, 0.5, 0.0, 2.0 ] );
var ALPHAR = new Float64Array( 2 );
var ALPHAI = new Float64Array( 2 );
var BETA = new Float64Array( 2 );
var Q = new Float64Array( 4 );
var Z = new Float64Array( 4 );
var WORK = new Float64Array( 10 );

var info = dhgeqz.ndarray( 'eigenvalues', 'none', 'none', 2, 0, 1,
    H, 2, 1, 0, T, 2, 1, 0,
    ALPHAR, 1, 0, ALPHAI, 1, 0, BETA, 1, 0,
    Q, 2, 1, 0, Z, 2, 1, 0,
    WORK, 1, 0, 2 );
// info => 0
```

The function has the following parameters:

-   **job**: `'eigenvalues'` to compute eigenvalues only, or `'schur'` to compute the generalized Schur form.
-   **compq**: `'none'` to not compute Q, `'initialize'` to initialize Q to the identity, or `'update'` to update a pre-existing Q.
-   **compz**: `'none'` to not compute Z, `'initialize'` to initialize Z to the identity, or `'update'` to update a pre-existing Z.
-   **N**: order of the matrices H and T.
-   **ilo**: start index of the balanced submatrix (0-based).
-   **ihi**: end index of the balanced submatrix (0-based).
-   **H**: upper Hessenberg matrix.
-   **strideH1**: stride of the first dimension of `H`.
-   **strideH2**: stride of the second dimension of `H`.
-   **offsetH**: starting index for `H`.
-   **T**: upper triangular matrix.
-   **strideT1**: stride of the first dimension of `T`.
-   **strideT2**: stride of the second dimension of `T`.
-   **offsetT**: starting index for `T`.
-   **ALPHAR**: real parts of generalized eigenvalues.
-   **strideALPHAR**: stride for `ALPHAR`.
-   **offsetALPHAR**: starting index for `ALPHAR`.
-   **ALPHAI**: imaginary parts of generalized eigenvalues.
-   **strideALPHAI**: stride for `ALPHAI`.
-   **offsetALPHAI**: starting index for `ALPHAI`.
-   **BETA**: scaling factors for eigenvalues.
-   **strideBETA**: stride for `BETA`.
-   **offsetBETA**: starting index for `BETA`.
-   **Q**: left Schur vectors.
-   **strideQ1**: first dimension stride of `Q`.
-   **strideQ2**: second dimension stride of `Q`.
-   **offsetQ**: starting index for `Q`.
-   **Z**: right Schur vectors.
-   **strideZ1**: first dimension stride of `Z`.
-   **strideZ2**: second dimension stride of `Z`.
-   **offsetZ**: starting index for `Z`.
-   **WORK**: workspace array.
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **lwork**: workspace size.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The generalized eigenvalues are `(ALPHAR(j) + i*ALPHAI(j)) / BETA(j)`, for j = 0, ..., N-1.
-   `H` must be in upper Hessenberg form and `T` must be in upper triangular form on entry.
-   The function returns INFO: 0 on success, j+1 if the j-th eigenvalue failed to converge, N+1 or N+2 for unexpected failures.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dhgeqz = require( '@stdlib/lapack/base/dhgeqz' );

var H = new Float64Array( [ 2.0, 3.0, 1.5, 4.0 ] );
var T = new Float64Array( [ 1.0, 0.5, 0.0, 2.0 ] );
var ALPHAR = new Float64Array( 2 );
var ALPHAI = new Float64Array( 2 );
var BETA = new Float64Array( 2 );
var Q = new Float64Array( 4 );
var Z = new Float64Array( 4 );
var WORK = new Float64Array( 10 );

var info = dhgeqz.ndarray( 'eigenvalues', 'none', 'none', 2, 0, 1,
    H, 2, 1, 0, T, 2, 1, 0,
    ALPHAR, 1, 0, ALPHAI, 1, 0, BETA, 1, 0,
    Q, 2, 1, 0, Z, 2, 1, 0,
    WORK, 1, 0, 2 );

console.log( 'info:', info );
console.log( 'ALPHAR:', ALPHAR );
console.log( 'BETA:', BETA );
```

</section>

<!-- /.examples -->

<section class="related">

</section>

<!-- /.related -->

<section class="links">

</section>

<!-- /.links -->
